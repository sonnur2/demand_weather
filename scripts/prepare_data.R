# Shared data preparation for the demand-temperature regressions.
#
# Builds, for 2021-06-01 to 2025-12-31 (blackout days dropped):
#   pen     hourly peninsular demand (MW) with demand-weighted national HDD / CDD
#   prov    hourly small-consumer demand by province (kWh) with province HDD / CDD
#   wx      daily weather by province
#   w       province weights (mean small-consumer demand share)
#
# Sourced by the regression scripts; run them from the repository root.

invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "Spanish_Spain.utf8" else "C.UTF-8"))

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(fixest)
})

raw_dir <- "raw_data"

sample_start <- as.IDate("2021-06-01")
sample_end   <- as.IDate("2025-12-31")
drop_dates   <- as.IDate(c("2025-04-28", "2025-04-29"))  # Iberian blackout

# Upper case, no accents, so demand and weather province names match
norm_name <- function(x) {
  x <- toupper(trimws(enc2utf8(x)))
  chartr("\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1\u00c0\u00c8\u00d2\u00c7", "AEIOUUNAEOC", x)
}

# ---------------------------------------------------------------------------
# Holidays
# ---------------------------------------------------------------------------

easter_sunday <- function(y) {
  # Anonymous Gregorian algorithm
  a <- y %% 19; b <- y %/% 100; c <- y %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3; h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4; l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day <- ((h + l - 7 * m + 114) %% 31) + 1
  as.IDate(sprintf("%d-%02d-%02d", y, month, day))
}

# National holidays common to the whole peninsula, plus Holy Thursday, which
# is a holiday in every peninsular community except Cataluna (and some years
# Comunidad Valenciana). Regional holidays and Sunday-to-Monday transfers are
# read from raw_data/holidays_regional.csv when that file exists
# (columns: date, province; province = "ALL" for a peninsula-wide day).
national_holidays <- function(years) {
  fixed <- c("01-01", "01-06", "05-01", "08-15", "10-12", "11-01", "12-06", "12-08", "12-25")
  rbindlist(lapply(years, function(y) {
    es <- easter_sunday(y)
    data.table(
      date = c(as.IDate(paste0(y, "-", fixed)), es - 2L, es - 3L),
      type = c(rep("national", length(fixed)), "good_friday", "holy_thursday")
    )
  }))
}

hol <- national_holidays(2021:2025)
regional_file <- file.path(raw_dir, "holidays_regional.csv")
regional <- if (file.exists(regional_file)) {
  r <- fread(regional_file)
  r[, `:=`(date = as.IDate(date), province = norm_name(province))]
  r
} else NULL

# ---------------------------------------------------------------------------
# Weather: daily HDD / CDD by province
# ---------------------------------------------------------------------------

wx_file <- file.path(raw_dir, "HDD14_CDD21.xlsx")
wx_sheets <- grep("^[0-9]{2}_", excel_sheets(wx_file), value = TRUE)
wx <- rbindlist(lapply(wx_sheets, function(s) {
  d <- as.data.table(read_excel(wx_file, sheet = s))
  setnames(d, c("date", "tmin", "tmax", "tavg", "hdd", "cdd"))
  d[, province := norm_name(sub("^[0-9]{2}_", "", s))]
}))
wx[, date := as.IDate(date)]

# ---------------------------------------------------------------------------
# Small-consumer demand by province (kWh, hourly)
# ---------------------------------------------------------------------------

prov_file <- file.path(raw_dir, "hourly_demand_province_2018_2025.xlsx")
prov <- rbindlist(lapply(excel_sheets(prov_file), function(s) {
  # Row 1: column titles, row 2: province names, data from row 3
  names_row <- unlist(read_excel(prov_file, sheet = s, range = cell_rows(2), col_names = FALSE,
                                 .name_repair = "minimal"))
  names_row <- names_row[!is.na(names_row)]
  d <- as.data.table(read_excel(prov_file, sheet = s, skip = 2, col_names = FALSE,
                                .name_repair = "minimal"))
  setnames(d, c("fecha", "dia", "mes", "ano", "hora", norm_name(names_row)))
  d <- d[!is.na(fecha)]
  d <- melt(d[, c(1, 5, 6:ncol(d)), with = FALSE], id.vars = c("fecha", "hora"),
            variable.name = "province", value.name = "demand", variable.factor = FALSE)
  d
}))
prov[, `:=`(date = as.IDate(fecha), hour = as.integer(hora), demand = as.numeric(demand))]
prov[, c("fecha", "hora") := NULL]

missing_wx <- setdiff(unique(prov$province), unique(wx$province))
if (length(missing_wx)) stop("No weather for provinces: ", paste(missing_wx, collapse = ", "))

# ---------------------------------------------------------------------------
# Peninsular demand (MW, hourly). Community columns are peninsular demand
# times a monthly share, so only the peninsula column is used.
# ---------------------------------------------------------------------------

pen <- fread(file.path(raw_dir, "hourly_demand_community_2015_2025.csv"),
             select = c("datetime", "peninsula"), colClasses = list(character = "datetime"))
pen[, local := as.POSIXct(substr(datetime, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")]
pen[, `:=`(date = as.IDate(local), hour = as.integer(format(local, "%H")), demand = peninsula)]
pen <- pen[, .(demand = mean(demand)), by = .(date, hour)]  # merges any repeated DST hour

# National weather: province HDD / CDD weighted by mean small-consumer demand
w <- prov[date >= sample_start & date <= sample_end, .(w = mean(demand)), by = province]
w[, w := w / sum(w)]
wx_nat <- merge(wx, w, by = "province")[
  , .(hdd = sum(w * hdd), cdd = sum(w * cdd), tavg = sum(w * tavg)), by = date]

# ---------------------------------------------------------------------------
# Calendar variables
# ---------------------------------------------------------------------------

add_calendar <- function(d, hol_dates) {
  d[, `:=`(
    year   = year(date),
    month  = month(date),
    ym     = year(date) * 100L + month(date),
    week   = format(as.Date(date), "%G-%V"),       # ISO year-week
    dow    = wday(date),                         # 1 = Sunday
    holiday = as.integer(date %in% hol_dates)
  )]
  # Day type: day of week, with holidays as their own type (load shape like Sunday)
  d[, daytype := fifelse(holiday == 1L, "holiday", as.character(dow))]
  d
}

nat_hol_dates <- hol$date
if (!is.null(regional)) nat_hol_dates <- c(nat_hol_dates, regional[province == "ALL", date])

pen <- merge(pen, wx_nat, by = "date")
pen <- pen[date >= sample_start & date <= sample_end & !date %in% drop_dates]
pen <- add_calendar(pen, nat_hol_dates)
pen[, ldemand := log(demand)]

prov <- merge(prov, wx[, .(date, province, hdd, cdd, tavg)], by = c("date", "province"))
prov <- prov[date >= sample_start & date <= sample_end & !date %in% drop_dates]
prov <- add_calendar(prov, nat_hol_dates)
if (!is.null(regional)) {
  reg_keys <- regional[province != "ALL", .(date, province, reg = 1L)]
  prov <- merge(prov, reg_keys, by = c("date", "province"), all.x = TRUE)
  prov[reg == 1L, `:=`(holiday = 1L, daytype = "holiday")]
  prov[, reg := NULL]
}
prov[, ldemand := log(demand)]
prov[, w_dem := mean(demand), by = province]

cat(sprintf("Peninsula: %d hours, %s to %s\n", nrow(pen), min(pen$date), max(pen$date)))
cat(sprintf("Provinces: %d province-hours, %d provinces\n", nrow(prov), uniqueN(prov$province)))
