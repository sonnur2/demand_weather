# Hourly electricity demand in 2030 under projected weather, for every specification
#
# Every specification estimated so far is re-estimated on 2021-06 to 2025-12 and
# applied to daily HDD14 / CDD21 brought to the 2030 climate (ERA5 + CMIP6
# correction, from the AlphaKlima side).
#
# Peninsula (MW), log hourly demand:
#   M1  HDD, CDD (small-consumer weights) | hour + day type + month + year
#   M2  HDD, CDD (small-consumer weights) | hour x month + hour x day type + year
#   M3  HDD, CDD (small-consumer weights) | hour x month + hour x day type + year-month
#   M4  M2 with HDD, CDD effects by hour of day
#   N2  national HDD, CDD (total-demand weights), M2 fixed effects
#   N4  N2 with effects by hour of day
#   Z2  four climate-zone HDD, CDD, M2 fixed effects
#   Z4  Z2 with effects by hour of day
# Small consumers by province (kWh), log hourly demand, province's own HDD, CDD:
#   M1  | province + hour + day type + month + year
#   M2  | province x hour x month + province x hour x day type + province x year
#   M3  | province x hour x month + province x hour x day type + province x year-month
#   M2w M2 weighted by mean province demand
#   M4  M2 with effects by hour of day
#
# Weather scenarios, all on the 2030 calendar:
#   std         2030 "standard" year, built from 2024 (near the median)
#   extreme     2030 extreme-summer year, built from 1991 (about 1 in 50)
#   normal      average 2018-2025 weather for each province and calendar day
#   no_weather  HDD = CDD = 0 everywhere
#   event_1..8  standard year with one heat event (heat_events_2030.xlsx) on its dates
#
# Level: year effects of `level_year` (2025); year-month effects of the same month
# of `level_year` (M3). National holidays of 2030; 24 hours a day (no DST shift).
# Log predictions are converted to levels with Duan's smearing factor per model.
#
# Run from the repository root:  Rscript scripts/project_2030.R

source("scripts/prepare_data.R")
source("scripts/zone_setup.R")
library(writexl)

out_dir <- "output/projections_2030"
dir.create(file.path(out_dir, "provinces"), recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(4)

level_year <- 2025
zones <- c("north", "interior", "med", "south")
zt <- function(v, z = zones) paste0(v, "_", z)
hourly <- function(vars) paste(sprintf("i(hour, %s)", vars), collapse = " + ")

# Peninsula weather: small-consumer-weighted national (hdd, cdd), total-demand
# weighted national (hdd_nat, cdd_nat) and zone HDD / CDD
pen_weather <- function(x) {
  nat_small <- merge(x, w, by = "province")[, .(hdd = sum(w * hdd), cdd = sum(w * cdd)), by = date]
  merge(nat_small, zone_weather(x), by = "date")
}

# ---------------------------------------------------------------------------
# Estimation
# ---------------------------------------------------------------------------

d <- merge(pen[, !c("hdd", "cdd", "tavg")],
           pen_weather(wx[date >= sample_start & date <= sample_end]), by = "date")

fe2 <- "| hour^month + hour^daytype + year"
pen_specs <- list(
  M1 = "hdd + cdd | hour + daytype + month + year",
  M2 = paste("hdd + cdd", fe2),
  M3 = "hdd + cdd | hour^month + hour^daytype + ym",
  M4 = paste(hourly(c("hdd", "cdd")), fe2),
  N2 = paste("hdd_nat + cdd_nat", fe2),
  N4 = paste(hourly(c("hdd_nat", "cdd_nat")), fe2),
  Z2 = paste(paste(c(zt("hdd"), zt("cdd")), collapse = " + "), fe2),
  Z4 = paste(hourly(c(zt("hdd"), zt("cdd"))), fe2)
)
pen_models <- lapply(pen_specs, function(s) feols(as.formula(paste("ldemand ~", s)), d, cluster = ~week))

pfe2 <- "| province^hour^month + province^hour^daytype + province^year"
prov_specs <- list(
  M1  = "hdd + cdd | province + hour + daytype + month + year",
  M2  = paste("hdd + cdd", pfe2),
  M3  = "hdd + cdd | province^hour^month + province^hour^daytype + province^ym",
  M2w = paste("hdd + cdd", pfe2),
  M4  = paste(hourly(c("hdd", "cdd")), pfe2)
)
prov_models <- lapply(names(prov_specs), function(k) {
  feols(as.formula(paste("ldemand ~", prov_specs[[k]])), prov, cluster = ~province + week,
        weights = if (k == "M2w") ~w_dem else NULL)
})
names(prov_models) <- names(prov_specs)

smear <- function(m) mean(exp(resid(m)))
pen_smear <- sapply(pen_models, smear)
prov_smear <- sapply(prov_models, smear)

# In-sample fit by specification (small consumers: hourly sum of provinces)
fit_tab <- rbind(
  rbindlist(lapply(names(pen_models), function(k) {
    f <- exp(predict(pen_models[[k]])) * pen_smear[k]
    data.table(series = "peninsula", spec = k, r2 = fitstat(pen_models[[k]], "r2")[[1]],
               mape_hourly_pct = 100 * mean(abs(f / d$demand - 1)))
  })),
  rbindlist(lapply(names(prov_models), function(k) {
    f <- exp(predict(prov_models[[k]])) * prov_smear[k]
    tot <- data.table(prov[, .(date, hour, demand)], f)[, .(a = sum(demand), f = sum(f)), by = .(date, hour)]
    data.table(series = "small consumers", spec = k, r2 = fitstat(prov_models[[k]], "r2")[[1]],
               mape_hourly_pct = 100 * mean(abs(tot$f / tot$a - 1)))
  }))
)

# ---------------------------------------------------------------------------
# 2030 weather inputs (daily, by province)
# ---------------------------------------------------------------------------

read_year_file <- function(path) {
  sheets <- grep("^[0-9]{2}_", excel_sheets(path), value = TRUE)
  rbindlist(lapply(sheets, function(s) {
    x <- as.data.table(read_excel(path, sheet = s))
    setnames(x, c("date", "tmin", "tmax", "tavg", "hdd", "cdd"))
    x[, .(date = as.IDate(date), province = norm_name(sub("^[0-9]{2}_", "", s)), hdd, cdd)]
  }))
}
wx_std <- read_year_file(file.path(raw_dir, "province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx"))
wx_ext <- read_year_file(file.path(raw_dir, "province_daily_degree_days_HDD14_CDD21_2030_from_1991.xlsx"))
stopifnot(setequal(unique(wx_std$province), wt$province), setequal(unique(wx_ext$province), wt$province))
dates_2030 <- sort(unique(wx_std$date))

wx_normal <- wx[format(date, "%m-%d") != "02-29",
                .(hdd = mean(hdd), cdd = mean(cdd)), by = .(province, md = format(date, "%m-%d"))]
wx_normal <- wx_normal[, .(date = as.IDate(paste0("2030-", md)), province, hdd, cdd)]
wx_none <- CJ(date = dates_2030, province = wt$province)[, `:=`(hdd = 0, cdd = 0)]

ev_file <- file.path(raw_dir, "heat_events_2030.xlsx")
ev_summary <- as.data.table(read_excel(ev_file, sheet = "Summary"))
setnames(ev_summary, 1:3, c("id", "event", "return_period"))
ev_summary <- ev_summary[!is.na(event), .(id = as.integer(id), event, return_period)]
events <- rbindlist(lapply(setdiff(excel_sheets(ev_file), "Summary"), function(s) {
  x <- as.data.table(read_excel(ev_file, sheet = s))
  setnames(x, 1, "date")
  x <- melt(x[, !grepl("^Mainland", names(x)), with = FALSE], id.vars = "date", variable.factor = FALSE)
  x[, `:=`(province = norm_name(sub(" (CDD21|HDD14) \\(.*$", "", variable)),
           var = ifelse(grepl("CDD21", variable), "cdd", "hdd"))]
  x <- dcast(x, date + province ~ var, value.var = "value")
  x[, `:=`(date = as.IDate(date), id = as.integer(sub("^([0-9]+) .*", "\\1", s)))]
}))
stopifnot(setequal(unique(events$province), wt$province))

scenarios <- list(std = wx_std, extreme = wx_ext, normal = wx_normal, no_weather = wx_none)
for (e in sort(unique(events$id))) {
  ev <- events[id == e]
  scenarios[[paste0("event_", e)]] <- rbind(wx_std[!date %in% ev$date], ev[, .(date, province, hdd, cdd)])
}
event_dates <- events[, .(date = unique(date)), by = id]

# ---------------------------------------------------------------------------
# Projection
# ---------------------------------------------------------------------------

hol_2030 <- national_holidays(2030)$date
cal <- add_calendar(CJ(date = dates_2030, hour = 0:23), hol_2030)
cal[, `:=`(year = level_year, ym = level_year * 100L + month)]

pcal <- add_calendar(CJ(date = dates_2030, hour = 0:23, province = wt$province), hol_2030)
pcal[, `:=`(year = level_year, ym = level_year * 100L + month)]

project_pen <- function(wx_daily) {
  x <- merge(cal, pen_weather(wx_daily), by = "date")
  setorder(x, date, hour)
  out <- x[, .(date, hour)]
  for (k in names(pen_models)) out[, (k) := exp(predict(pen_models[[k]], newdata = x)) * pen_smear[k]]
  out
}
project_prov <- function(wx_daily) {
  x <- merge(pcal, wx_daily, by = c("date", "province"))
  setorder(x, date, hour, province)
  out <- x[, .(date, hour, province)]
  for (k in names(prov_models)) out[, (k) := exp(predict(prov_models[[k]], newdata = x)) * prov_smear[k]]
  out
}

pen_cols <- names(pen_models)
sc_cols <- paste0("sc_", names(prov_models))
res <- list()
for (s in names(scenarios)) {
  pp <- project_pen(scenarios[[s]])
  pv <- project_prov(scenarios[[s]])
  if (s %in% c("std", "extreme", "normal")) {
    pv_out <- copy(pv)[, (names(prov_models)) := lapply(.SD, round), .SDcols = names(prov_models)]
    fwrite(cbind(datetime = sprintf("%s %02d:00", pv$date, pv$hour), pv_out),
           file.path(out_dir, "provinces", sprintf("small_consumers_kwh_2030_%s.csv.gz", s)), compress = "gzip")
  }
  tot <- pv[, lapply(.SD, function(v) sum(v) / 1000), by = .(date, hour), .SDcols = names(prov_models)]
  setnames(tot, names(prov_models), sc_cols)
  res[[s]] <- merge(pp, tot, by = c("date", "hour"))[, scenario := s]
}
all <- rbindlist(res)
all[, datetime := sprintf("%s %02d:00", date, hour)]
setcolorder(all, c("scenario", "datetime", "date", "hour", pen_cols, sc_cols))

# Event rows only on the event's own dates
ev_rows <- rbindlist(lapply(sort(unique(events$id)), function(e) {
  all[scenario == paste0("event_", e) & date %in% event_dates[id == e, date]][, id := e]
}))
ev_rows <- merge(ev_rows, ev_summary, by = "id")
ev_out_cols <- c("id", "event", "return_period", "datetime", "date", "hour", pen_cols, sc_cols)

all[, (c(pen_cols, sc_cols)) := lapply(.SD, round, 1), .SDcols = c(pen_cols, sc_cols)]
year_rows <- all[scenario %in% c("std", "extreme", "normal", "no_weather")]
fwrite(year_rows, file.path(out_dir, "demand_hourly_2030.csv"))
fwrite(ev_rows[, ..ev_out_cols], file.path(out_dir, "demand_hourly_2030_events.csv"))

# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------

split_spec <- function(x) {
  x[, `:=`(series = ifelse(startsWith(spec, "sc_"), "small consumers", "peninsula"), spec = sub("^sc_", "", spec))]
}
long <- melt(year_rows, id.vars = c("scenario", "date", "hour"), measure.vars = c(pen_cols, sc_cols),
             variable.name = "spec", value.name = "mw", variable.factor = FALSE)
split_spec(long)
year_tab <- long[, .(twh = sum(mw) / 1e6, peak_mw = max(mw), summer_twh = sum(mw[month(date) %in% 6:8]) / 1e6),
                 by = .(series, spec, scenario)]
year_tab <- dcast(year_tab, series + spec ~ scenario, value.var = c("twh", "peak_mw", "summer_twh"))
year_tab[, `:=`(weather_twh_std = twh_std - twh_no_weather,
                weather_twh_extreme = twh_extreme - twh_no_weather,
                std_vs_normal_pct = 100 * (twh_std / twh_normal - 1),
                extreme_vs_normal_pct = 100 * (twh_extreme / twh_normal - 1),
                summer_extreme_vs_normal_pct = 100 * (summer_twh_extreme / summer_twh_normal - 1))]
setorder(year_tab, series, spec)

ref <- all[scenario %in% c("std", "normal")]
ev_long <- rbindlist(lapply(c(pen_cols, sc_cols), function(k) {
  x <- merge(ev_rows[, .(id, event, return_period, date, hour, ev = get(k))],
             dcast(ref[, .(scenario, date, hour, v = get(k))], date + hour ~ scenario, value.var = "v"),
             by = c("date", "hour"))
  x[, .(spec = k, days = uniqueN(date),
        pct_vs_normal = 100 * (sum(ev) / sum(normal) - 1),
        pct_vs_std = 100 * (sum(ev) / sum(std) - 1),
        extra_gwh_vs_normal = sum(ev - normal) / 1e3,
        extra_gwh_vs_std = sum(ev - std) / 1e3,
        peak_event_mw = max(ev), peak_std_mw = max(std), peak_normal_mw = max(normal)),
    by = .(id, event, return_period)]
}))
split_spec(ev_long)
setcolorder(ev_long, c("series", "spec"))
setorder(ev_long, series, spec, id)

fwrite(year_tab, file.path(out_dir, "summary_by_spec_year.csv"))
fwrite(ev_long, file.path(out_dir, "summary_by_spec_events.csv"))
fwrite(fit_tab, file.path(out_dir, "fit_by_spec.csv"))

readme <- data.table(item = c(
  "Units", "Peninsula columns", "Small-consumer columns", "Scenarios", "Level", "Calendar",
  "Province detail", "Code"), description = c(
  "MW (hourly mean). Small-consumer columns are the sum of the 47 provinces (kWh per hour / 1000).",
  "M1-M4: baseline specs with national HDD/CDD (small-consumer weights). N2, N4: national HDD/CDD with total-demand weights. Z2, Z4: four climate zones. Specs ending in 4 have HDD/CDD effects by hour of day.",
  "sc_M1, sc_M2, sc_M3, sc_M2w, sc_M4: small-consumer province specs, each province with its own HDD/CDD.",
  "std = 2030 from 2024 (median year); extreme = 2030 from 1991 (extreme summer, about 1 in 50); normal = 2018-2025 average weather by calendar day; no_weather = HDD = CDD = 0; events = standard year with one heat event on its 2030 dates.",
  sprintf("Year effects of %d (M3: year-month effect of the same month of %d). Only weather changes; no structural demand growth.", level_year, level_year),
  "2030 dates, national holidays of 2030, 24 hours per day (no daylight-saving shift).",
  "output/projections_2030/provinces/small_consumers_kwh_2030_<scenario>.csv.gz (kWh by province and hour)",
  "scripts/project_2030.R"))

sheet_wide <- function(s) year_rows[scenario == s, c("datetime", "date", "hour", pen_cols, sc_cols), with = FALSE]
write_xlsx(list(README = readme, Summary_year = year_tab, Summary_events = ev_long, Fit = fit_tab,
                Hourly_std = sheet_wide("std"), Hourly_extreme = sheet_wide("extreme"),
                Hourly_normal = sheet_wide("normal"), Hourly_no_weather = sheet_wide("no_weather"),
                Hourly_events = ev_rows[, ..ev_out_cols]),
           file.path(out_dir, "demand_projections_2030.xlsx"))

sink(file.path(out_dir, "projection_summary.txt"))
cat("Level: year effects of", level_year, "\n\nIn-sample fit:\n"); print(fit_tab, digits = 4)
cat("\n== 2030 by specification (TWh, peak MW) ==\n")
print(year_tab[, .(series, spec, twh_no_weather, twh_normal, twh_std, twh_extreme,
                   peak_mw_normal, peak_mw_std, peak_mw_extreme, std_vs_normal_pct, extreme_vs_normal_pct,
                   summer_extreme_vs_normal_pct)], digits = 4)
cat("\n== Heat events: % vs normal weather on the same dates ==\n")
print(dcast(ev_long, id + event ~ series + spec, value.var = "pct_vs_normal"), digits = 3)
cat("\n== Heat events: peak MW increase vs standard year on the same dates ==\n")
print(dcast(ev_long[, .(series, spec, id, event, d = peak_event_mw - peak_std_mw)], id + event ~ series + spec,
            value.var = "d"), digits = 4)
sink()

# Daily peak demand through the year, peninsula specs, standard scenario
dp <- year_rows[scenario == "std", lapply(.SD, max), by = date, .SDcols = pen_cols]
png(file.path(out_dir, "daily_peak_2030_by_spec.png"), width = 1800, height = 750, res = 150)
par(mar = c(4, 4.5, 2.5, 1))
pal <- c(M1 = "#BFC5CE", M2 = "#8A8F98", M3 = "#5B6470", M4 = "#2E3440",
         N2 = "#9CC3E6", N4 = "#5B8DB8", Z2 = "#E8A24A", Z4 = "#C2571A")
rng <- range(dp[, ..pen_cols]) / 1e3
plot(dp$date, dp$M2 / 1e3, type = "n", ylim = c(rng[1], rng[2] + 2.5),
     xlab = "", ylab = "Daily peak demand (GW)", main = "Projected daily peak, 2030 standard year, by specification")
for (k in pen_cols) lines(dp$date, dp[[k]] / 1e3, col = pal[k], lwd = 1.2)
legend("top", horiz = TRUE, pen_cols, col = pal[pen_cols], lwd = 2, bty = "n", cex = 0.85)
dev.off()

cat("Done. Results in", out_dir, "\n")
