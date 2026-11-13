# Hourly peninsular demand in 2030 under projected weather
#
# Model: zone M4 (Z4) estimated on 2021-06 to 2025-12, i.e. log hourly demand on
# zone HDD / CDD interacted with hour of day, with hour x month, hour x day type
# and year fixed effects. It is applied to daily HDD14 / CDD21 brought to the 2030
# climate (ERA5 + CMIP6 correction, provided by the AlphaKlima side):
#   std      2030 "standard" year, built from 2024 (near the median)
#   extreme  2030 extreme-summer year, built from 1991 (about 1 in 50)
#   events   eight heat episodes (raw_data/heat_events_2030.xlsx), each placed
#            into the standard year on its own 2030 dates (stress tests)
# plus two references on the same 2030 calendar:
#   normal      average 2018-2025 weather for each province and calendar day
#   no_weather  HDD = CDD = 0 everywhere (demand with no heating or cooling load)
#
# Level: the model explains how weather moves demand, not underlying growth.
# The 2030 level uses the year fixed effect of `level_year` (2025), optionally
# rescaled so the standard year sums to `target_twh`.
#
# Calendar: 2030 dates, national holidays of 2030, 24 hours a day (no DST shift).
#
# Run from the repository root:  Rscript scripts/project_2030.R

source("scripts/prepare_data.R")
source("scripts/zone_setup.R")

out_dir <- "output/projections_2030"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(4)

level_year <- 2025
target_twh <- NULL   # e.g. 260 to scale the standard year to 260 TWh

zones <- c("north", "interior", "med", "south")
zt <- function(v, z = zones) paste0(v, "_", z)

# ---------------------------------------------------------------------------
# Estimate Z4
# ---------------------------------------------------------------------------

d <- merge(pen[, !c("hdd", "cdd", "tavg")],
           zone_weather(wx[date >= sample_start & date <= sample_end]), by = "date")
rhs <- paste(sprintf("i(hour, %s)", c(zt("hdd"), zt("cdd"))), collapse = " + ")
z4 <- feols(as.formula(paste("ldemand ~", rhs, "| hour^month + hour^daytype + year")),
            d, cluster = ~week)
smear <- mean(exp(resid(z4)))   # Duan retransformation from log to MW

# In-sample check: fitted vs actual demand
d[, fit := exp(predict(z4)) * smear]
fit_check <- d[, .(mape_hourly = mean(abs(fit / demand - 1)), bias = sum(fit) / sum(demand) - 1)]
fit_year <- d[, .(actual_twh = sum(demand) / 1e6, fitted_twh = sum(fit) / 1e6,
                  actual_peak = max(demand), fitted_peak = max(fit)), by = year]

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

# Normal weather: 2018-2025 mean for each province and calendar day
wx_normal <- wx[format(date, "%m-%d") != "02-29",
                .(hdd = mean(hdd), cdd = mean(cdd)), by = .(province, md = format(date, "%m-%d"))]
wx_normal <- wx_normal[, .(date = as.IDate(paste0("2030-", md)), province, hdd, cdd)]

wx_none <- CJ(date = dates_2030, province = wt$province)[, `:=`(hdd = 0, cdd = 0)]

# Heat events: one sheet per event, mainland and province columns per day
ev_file <- file.path(raw_dir, "heat_events_2030.xlsx")
ev_summary <- as.data.table(read_excel(ev_file, sheet = "Summary"))
setnames(ev_summary, 1:3, c("id", "event", "return_period"))
ev_summary <- ev_summary[!is.na(event)]
ev_sheets <- setdiff(excel_sheets(ev_file), "Summary")
events <- rbindlist(lapply(ev_sheets, function(s) {
  x <- as.data.table(read_excel(ev_file, sheet = s))
  setnames(x, 1, "date")
  x <- melt(x[, !grepl("^Mainland", names(x)), with = FALSE], id.vars = "date",
            variable.factor = FALSE)
  x[, `:=`(province = norm_name(sub(" (CDD21|HDD14) \\(.*$", "", variable)),
           var = ifelse(grepl("CDD21", variable), "cdd", "hdd"))]
  x <- dcast(x, date + province ~ var, value.var = "value")
  x[, `:=`(date = as.IDate(date), sheet = s, id = as.integer(sub("^([0-9]+) .*", "\\1", s)))]
}))
stopifnot(setequal(unique(events$province), wt$province))

# ---------------------------------------------------------------------------
# Projection
# ---------------------------------------------------------------------------

hol_2030 <- national_holidays(2030)$date
cal <- CJ(date = dates_2030, hour = 0:23)
cal <- add_calendar(cal, hol_2030)
cal[, year := level_year]   # 2030 level = level_year fixed effect

project <- function(wx_daily) {
  x <- merge(cal, zone_weather(wx_daily), by = "date")
  setorder(x, date, hour)
  exp(predict(z4, newdata = x)) * smear
}

proj <- cal[, .(date, hour, month, daytype)]
setorder(proj, date, hour)
proj[, `:=`(
  demand_std        = project(wx_std),
  demand_extreme    = project(wx_ext),
  demand_normal     = project(wx_normal),
  demand_no_weather = project(wx_none)
)]

scale <- if (is.null(target_twh)) 1 else target_twh / (sum(proj$demand_std) / 1e6)
dem_cols <- grep("^demand_", names(proj), value = TRUE)
proj[, (dem_cols) := lapply(.SD, `*`, scale), .SDcols = dem_cols]

# Events: standard year with the event's dates replaced
ev_hourly <- rbindlist(lapply(sort(unique(events$id)), function(e) {
  ev <- events[id == e]
  w_e <- rbind(wx_std[!date %in% ev$date], ev[, .(date, province, hdd, cdd)])
  out <- proj[date %in% ev$date, .(date, hour, demand_normal, demand_std)]
  full <- proj[, .(date, hour)][, demand_event := project(w_e) * scale]
  out <- merge(out, full, by = c("date", "hour"))
  out[, `:=`(id = e, event = ev_summary[as.integer(id) == e, event])]
}))

# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------

proj[, datetime := sprintf("%s %02d:00", date, hour)]
setcolorder(proj, c("datetime", "date", "hour"))
fwrite(proj[, c("datetime", "date", "hour", dem_cols), with = FALSE],
       file.path(out_dir, "hourly_demand_2030.csv"))
setcolorder(ev_hourly, c("id", "event", "date", "hour"))
fwrite(ev_hourly, file.path(out_dir, "hourly_demand_2030_events.csv"))

summ <- function(x, label) {
  data.table(scenario = label, twh = sum(x) / 1e6, mean_mw = mean(x), peak_mw = max(x))
}
summer <- proj$month %in% 6:8
year_tab <- rbind(summ(proj$demand_no_weather, "no weather (HDD = CDD = 0)"),
                  summ(proj$demand_normal, "normal weather (2018-2025 average)"),
                  summ(proj$demand_std, "2030 standard (from 2024)"),
                  summ(proj$demand_extreme, "2030 extreme summer (from 1991)"))
summer_tab <- rbind(summ(proj$demand_normal[summer], "normal weather"),
                    summ(proj$demand_std[summer], "2030 standard"),
                    summ(proj$demand_extreme[summer], "2030 extreme summer"))

ev_tab <- ev_hourly[, .(days = uniqueN(date),
                        pct_vs_normal = 100 * (sum(demand_event) / sum(demand_normal) - 1),
                        peak_normal_mw = max(demand_normal),
                        extra_gwh = sum(demand_event - demand_std) / 1e3,
                        mean_extra_mw = mean(demand_event - demand_std),
                        mean_extra_pct = 100 * (sum(demand_event) / sum(demand_std) - 1),
                        peak_std_mw = max(demand_std), peak_event_mw = max(demand_event)),
                    by = .(id, event)]
ev_tab <- merge(ev_tab, ev_summary[, .(id = as.integer(id), return_period)], by = "id")
ev_tab[, peak_extra_mw := peak_event_mw - peak_std_mw]

sink(file.path(out_dir, "projection_summary.txt"))
cat("Zone M4 model, level = year FE", level_year, "| scale factor", round(scale, 4),
    "| smearing", round(smear, 5), "\n")
cat(sprintf("In-sample fit 2021-06 to 2025: hourly MAPE %.2f%%, total bias %.2f%%\n",
            100 * fit_check$mape_hourly, 100 * fit_check$bias))
print(fit_year, digits = 5)
cat("\n== 2030, full year ==\n"); print(year_tab, digits = 5)
cat("\n== 2030, June-August ==\n"); print(summer_tab, digits = 5)
cat("\n== Heat events vs normal weather and vs the 2030 standard year, same dates ==\n"); print(ev_tab, digits = 4)
sink()
fwrite(year_tab, file.path(out_dir, "summary_year.csv"))
fwrite(ev_tab, file.path(out_dir, "summary_events.csv"))

# Daily peak demand through the year
dp <- proj[, lapply(.SD, max), by = date, .SDcols = dem_cols]
png(file.path(out_dir, "daily_peak_2030.png"), width = 1800, height = 750, res = 150)
par(mar = c(4, 4.5, 2.5, 1))
cols <- c(demand_normal = "#8A8F98", demand_std = "#1F4E79", demand_extreme = "#C2571A")
plot(dp$date, dp$demand_normal / 1e3, type = "n", ylim = c(min(dp[, names(cols), with = FALSE]) / 1e3, max(dp[, names(cols), with = FALSE]) / 1e3 + 2.5),
     xlab = "", ylab = "Daily peak demand (GW)", main = "Projected daily peak demand, 2030")
for (k in names(cols)) lines(dp$date, dp[[k]] / 1e3, col = cols[k], lwd = 1.6)
legend("top", horiz = TRUE, c("Normal weather (2018-2025 avg)", "2030 standard (from 2024)", "2030 extreme summer (from 1991)"),
       col = cols, lwd = 2, bty = "n", cex = 0.85)
dev.off()

cat("Done. Results in", out_dir, "\n")
