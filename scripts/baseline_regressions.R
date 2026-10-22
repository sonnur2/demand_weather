# Baseline regressions: hourly electricity demand on daily HDD / CDD
#
# Two demand series, always hourly:
#   (A) Peninsular aggregate demand (MW), one time series
#   (B) Small-consumer demand by province (kWh), 47-province panel
# Weather: daily HDD14 / CDD21 by province. For (A) they are averaged across
# provinces with weights equal to each province's mean small-consumer demand.
#
# Sample: 2021-06-01 to 2025-12-31 (after COVID and the June 2021 2.0TD tariff).
# The 28-29 April 2025 blackout days are dropped.
#
# Run from the repository root:  Rscript scripts/baseline_regressions.R

invisible(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "Spanish_Spain.utf8" else "C.UTF-8"))

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(fixest)
})

raw_dir <- "raw_data"
out_dir <- "output/baseline"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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
fwrite(w[order(-w)], file.path(out_dir, "province_weights.csv"))

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

# ---------------------------------------------------------------------------
# Regressions (log demand, coefficients x 100 = % change per degree-day)
#   M1  additive calendar FE: hour, day type, month, year
#   M2  hour x month, hour x day type, year            (main baseline)
#   M3  as M2 but year-month FE: only within-month weather shocks identify
#   M4  as M2 with HDD / CDD effects by hour of day
# Province versions interact every FE with province.
# SE: peninsula clustered by week; provinces two-way by province and week.
# ---------------------------------------------------------------------------

setFixest_nthreads(4)

pen_models <- list(
  M1 = feols(ldemand ~ hdd + cdd | hour + daytype + month + year, pen, cluster = ~week),
  M2 = feols(ldemand ~ hdd + cdd | hour^month + hour^daytype + year, pen, cluster = ~week),
  M3 = feols(ldemand ~ hdd + cdd | hour^month + hour^daytype + ym, pen, cluster = ~week)
)
pen_m4 <- feols(ldemand ~ i(hour, hdd) + i(hour, cdd) | hour^month + hour^daytype + year,
                pen, cluster = ~week)

prov_models <- list(
  M1 = feols(ldemand ~ hdd + cdd | province + hour + daytype + month + year,
             prov, cluster = ~province + week),
  M2 = feols(ldemand ~ hdd + cdd | province^hour^month + province^hour^daytype + province^year,
             prov, cluster = ~province + week),
  M3 = feols(ldemand ~ hdd + cdd | province^hour^month + province^hour^daytype + province^ym,
             prov, cluster = ~province + week),
  M2w = feols(ldemand ~ hdd + cdd | province^hour^month + province^hour^daytype + province^year,
              prov, cluster = ~province + week, weights = ~w_dem)
)
prov_m4 <- feols(ldemand ~ i(hour, hdd) + i(hour, cdd) |
                   province^hour^month + province^hour^daytype + province^year,
                 prov, cluster = ~province + week)

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

dict <- c(ldemand = "log demand", hdd = "HDD14 (°C·day)", cdd = "CDD21 (°C·day)",
          daytype = "day type", ym = "year-month")

sink(file.path(out_dir, "baseline_tables.txt"))
cat("== Peninsular aggregate demand (log MW), hourly ==\n")
print(etable(pen_models, dict = dict, digits = 5, fitstat = ~ n + r2 + wr2))
cat("\n== Small-consumer demand by province (log kWh), hourly ==\n")
cat("M2w weights provinces by mean demand.\n")
print(etable(prov_models, dict = dict, digits = 5, fitstat = ~ n + r2 + wr2))
sink()

etable(pen_models, dict = dict, tex = TRUE, file = file.path(out_dir, "peninsula.tex"), replace = TRUE)
etable(prov_models, dict = dict, tex = TRUE, file = file.path(out_dir, "province.tex"), replace = TRUE)

# Hour-by-hour effects (M4), in % per degree-day
hourly_effects <- function(m, sample_name) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "est", "se", "t", "p"))
  ct[, `:=`(var = sub("^hour::[0-9]+:", "", term),
            hour = as.integer(sub("^hour::([0-9]+):.*", "\\1", term)))]
  ct[, .(sample = sample_name, var, hour, pct = 100 * est,
         lo = 100 * (est - 1.96 * se), hi = 100 * (est + 1.96 * se))]
}
eff <- rbind(hourly_effects(pen_m4, "Peninsula (aggregate)"),
             hourly_effects(prov_m4, "Provinces (small consumers)"))
fwrite(eff, file.path(out_dir, "hourly_effects_M4.csv"))

png(file.path(out_dir, "hourly_effects_M4.png"), width = 1600, height = 700, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4.5, 2.5, 1))
cols <- c("Peninsula (aggregate)" = "#1f4e79", "Provinces (small consumers)" = "#c0504d")
for (v in c("cdd", "hdd")) {
  sub <- eff[var == v]
  plot(NA, xlim = c(0, 23), ylim = range(c(0, sub$lo, sub$hi)), xaxt = "n",
       xlab = "Hour of day (local time)", ylab = "% change in demand per degree-day",
       main = if (v == "cdd") "Cooling: CDD21" else "Heating: HDD14")
  axis(1, at = seq(0, 23, 3))
  abline(h = 0, col = "grey60")
  for (s in names(cols)) {
    x <- sub[sample == s][order(hour)]
    polygon(c(x$hour, rev(x$hour)), c(x$lo, rev(x$hi)), col = adjustcolor(cols[s], 0.2), border = NA)
    lines(x$hour, x$pct, col = cols[s], lwd = 2)
  }
  if (v == "cdd") legend("topleft", names(cols), col = cols, lwd = 2, bty = "n", cex = 0.85)
}
dev.off()

# Implied level effects at the sample mean (M2)
level <- rbind(
  data.table(sample = "Peninsula (aggregate)", unit = "MW per degree-day",
             var = c("hdd", "cdd"), pct = 100 * coef(pen_models$M2)[c("hdd", "cdd")],
             mean_demand = mean(pen$demand)),
  data.table(sample = "Provinces, all summed (small consumers)", unit = "MW per degree-day",
             var = c("hdd", "cdd"), pct = 100 * coef(prov_models$M2w)[c("hdd", "cdd")],
             mean_demand = prov[, sum(demand) / 1000, by = .(date, hour)][, mean(V1)])
)
level[, level_effect := pct / 100 * mean_demand]
fwrite(level, file.path(out_dir, "level_effects_M2.csv"))

cat("Done. Results in", out_dir, "\n")
