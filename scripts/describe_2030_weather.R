# Descriptives of the two 2030 weather scenarios (no regression involved)
#   std     = 2030 from 2024 (Eduardo)
#   extreme = 2030 from 1991 (Eduardo)
# compared with each other and with observed 2018-2025 weather.
# National series use the same small-consumer province weights (w) as M1-M4.
# Output: output/weather_2030_descriptives/

source("scripts/prepare_data.R")
source("scripts/zone_setup.R")
library(ggplot2)

out_dir <- "output/weather_2030_descriptives"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_full <- function(path) {
  sheets <- grep("^[0-9]{2}_", excel_sheets(path), value = TRUE)
  rbindlist(lapply(sheets, function(s) {
    x <- as.data.table(read_excel(path, sheet = s))
    setnames(x, c("date", "tmin", "tmax", "tavg", "hdd", "cdd"))
    x[, .(date = as.IDate(date), province = norm_name(sub("^[0-9]{2}_", "", s)), tmin, tmax, tavg, hdd, cdd)]
  }))
}
wx30 <- rbind(
  read_full(file.path(raw_dir, "province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx"))[, scenario := "standard"],
  read_full(file.path(raw_dir, "province_daily_degree_days_HDD14_CDD21_2030_from_1991.xlsx"))[, scenario := "extreme"]
)
hist <- copy(wx)[, scenario := paste0("obs_", year(date))]

nat <- function(x) merge(x, w, by = "province")[, .(
  tavg = sum(w * tavg), tmin = sum(w * tmin), tmax = sum(w * tmax),
  hdd = sum(w * hdd), cdd = sum(w * cdd)), by = .(scenario, date)]
n30 <- nat(wx30)
nh <- nat(hist)
n30[, `:=`(month = month(date), doy = yday(date))]
nh[, `:=`(month = month(date), doy = yday(date))]

# ---------------------------------------------------------------------------
# 1. Annual and monthly totals
# ---------------------------------------------------------------------------
annual <- rbind(n30, nh)[, .(HDD = sum(hdd), CDD = sum(cdd), tavg = mean(tavg),
                              days_hdd_gt8 = sum(hdd > 8), days_cdd_gt5 = sum(cdd > 5),
                              max_hdd = max(hdd), max_cdd = max(cdd)), by = scenario]
fwrite(annual, file.path(out_dir, "annual_totals.csv"))

monthly <- n30[, .(HDD = sum(hdd), CDD = sum(cdd), tavg = mean(tavg), tmin = mean(tmin)),
               by = .(scenario, month)]
mh <- nh[year(date) %in% 2018:2025, .(HDD = sum(hdd), CDD = sum(cdd), tavg = mean(tavg)),
         by = .(y = year(date), month)][, .(HDD_obs_mean = mean(HDD), HDD_obs_max = max(HDD),
                                            CDD_obs_mean = mean(CDD), CDD_obs_max = max(CDD),
                                            tavg_obs_mean = mean(tavg)), by = month]
monthly_w <- dcast(monthly, month ~ scenario, value.var = c("HDD", "CDD", "tavg", "tmin"))
monthly_w <- merge(monthly_w, mh, by = "month")
fwrite(monthly_w, file.path(out_dir, "monthly_totals.csv"))

# ---------------------------------------------------------------------------
# 2. Extreme days
# ---------------------------------------------------------------------------
top <- function(v, k = 10) n30[order(-get(v)), head(.SD, k), by = scenario][
  , .(scenario, date, tavg = round(tavg, 1), tmin = round(tmin, 1), hdd = round(hdd, 2), cdd = round(cdd, 2))]
fwrite(rbind(top("hdd")[, rank_by := "coldest (HDD)"], top("cdd")[, rank_by := "hottest (CDD)"]),
       file.path(out_dir, "top10_days.csv"))

# Cold spells: runs of consecutive days with national HDD above the 2018-2025 99th percentile
thr <- quantile(nh$hdd, 0.99)
spells <- n30[order(scenario, date)][, run := rleid(hdd > thr), by = scenario][hdd > thr,
  .(start = min(date), end = max(date), days = .N, hdd_total = sum(hdd), hdd_peak = max(hdd)), by = .(scenario, run)]
fwrite(spells[, !"run"], file.path(out_dir, "cold_spells.csv"))

# Days around the projected peaks
peak_days <- n30[date %in% as.IDate(c("2030-01-08", "2030-02-14", "2030-07-30"))]
fwrite(peak_days, file.path(out_dir, "peak_days_weather.csv"))

# Zone view of February
zfeb <- merge(wx30[month(date) == 2], wt[, .(province, zone, w = weight)], by = "province")
zfeb <- zfeb[, .(HDD = sum(w * hdd) / sum(w)), by = .(scenario, zone)]  # mean daily HDD in February
fwrite(dcast(zfeb, zone ~ scenario, value.var = "HDD"), file.path(out_dir, "february_mean_daily_hdd_by_zone.csv"))

# ---------------------------------------------------------------------------
# 3. Plots
# ---------------------------------------------------------------------------
band <- nh[year(date) %in% 2018:2025 & !(month == 2 & mday(date) == 29),
           .(lo = min(tavg), hi = max(tavg), mid = mean(tavg)), by = doy]
cols <- c(standard = "#C8551B", extreme = "#14213D")
p1 <- ggplot() +
  geom_ribbon(data = band, aes(doy, ymin = lo, ymax = hi), fill = "grey85") +
  geom_line(data = band, aes(doy, mid), colour = "grey55") +
  geom_line(data = n30, aes(doy, tavg, colour = scenario), linewidth = 0.5) +
  geom_hline(yintercept = c(14, 21), linetype = "dashed") +
  scale_colour_manual(values = cols) +
  labs(x = "Day of 2030", y = "National mean temperature (°C)",
       title = "2030 daily mean temperature: standard (2024-based) vs extreme (1991-based)",
       subtitle = "Grey band: min-max of 2018-2025 observed on the same calendar day; dashed: 14 °C and 21 °C thresholds") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "daily_temperature_2030.png"), p1, width = 11, height = 5, dpi = 150)

ml <- melt(monthly, id.vars = c("scenario", "month"), measure.vars = c("HDD", "CDD"))
p2 <- ggplot(ml, aes(factor(month), value, fill = scenario)) +
  geom_col(position = "dodge") + facet_wrap(~variable, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = cols) +
  labs(x = "Month", y = "Degree-days (national, sum over the month)",
       title = "Monthly heating and cooling degree-days, 2030 scenarios") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "monthly_degree_days_2030.png"), p2, width = 9, height = 6, dpi = 150)

cat("HDD 99th pct threshold (2018-2025):", round(thr, 2), "\n")
print(annual[order(scenario)], digits = 4)
print(monthly_w, digits = 3)
print(spells)
print(top("hdd", 8))
print(top("cdd", 5))
print(peak_days[order(scenario, date)], digits = 3)
print(dcast(zfeb, zone ~ scenario, value.var = "HDD"), digits = 3)
