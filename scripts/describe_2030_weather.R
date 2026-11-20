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
# 3. Plots (white background; every line and band named in the legend)
# ---------------------------------------------------------------------------
cols <- c("Standard 2030 (from 2024)" = "#D55E00", "Extreme 2030 (from 1991)" = "#0B3C8C",
          "Observed 2018-2025 average" = "grey35")
lab <- c(standard = "Standard 2030 (from 2024)", extreme = "Extreme 2030 (from 1991)")
n30[, Scenario := factor(lab[scenario], levels = lab)]
th <- theme_bw(base_size = 12) + theme(legend.position = "bottom", legend.box = "vertical",
                                       plot.background = element_rect(fill = "white", colour = NA))
save <- function(p, f, w = 11, h = 5.5) ggsave(file.path(out_dir, f), p, width = w, height = h, dpi = 150, bg = "white")
mticks <- yday(as.IDate(sprintf("2030-%02d-01", 1:12)))
xmonth <- scale_x_continuous(breaks = mticks, labels = month.abb, expand = c(0.01, 0))

obs <- nh[year(date) %in% 2018:2025 & !(month == 2 & mday(date) == 29)]
obs[, doy30 := yday(as.IDate(sprintf("2030-%02d-%02d", month, mday(date))))]
band <- obs[, .(lo = min(tavg), hi = max(tavg), mid = mean(tavg),
                hdd = mean(hdd), cdd = mean(cdd)), by = doy30][order(doy30)]

# (1) Daily national mean temperature
p1 <- ggplot() +
  geom_ribbon(data = band, aes(doy30, ymin = lo, ymax = hi, fill = "Observed 2018-2025: coldest to warmest year on that date")) +
  geom_line(data = band, aes(doy30, mid, colour = "Observed 2018-2025 average"), linewidth = 0.6) +
  geom_line(data = n30, aes(doy, tavg, colour = Scenario), linewidth = 0.6) +
  geom_hline(aes(yintercept = c(14, 21), linetype = c("HDD threshold 14 °C", "CDD threshold 21 °C"))) +
  scale_colour_manual(NULL, values = cols) + scale_fill_manual(NULL, values = "grey85") +
  scale_linetype_manual(NULL, values = c("dashed", "dotted")) + xmonth +
  labs(x = NULL, y = "Daily mean temperature, °C",
       title = "Daily national mean temperature in the two 2030 weather scenarios",
       subtitle = "National = demand-weighted average of the 47 provinces.\nDays below 14 °C count heating degree-days (HDD); days above 21 °C count cooling degree-days (CDD).") + th
save(p1, "1_daily_temperature_2030.png", h = 6.5)

# (2) Monthly HDD and CDD vs observed average
mobs <- obs[, .(HDD = sum(hdd), CDD = sum(cdd)), by = .(y = year(date), month)][
  , .(HDD = mean(HDD), CDD = mean(CDD)), by = month]
mm <- rbind(monthly[, .(Scenario = lab[scenario], month, HDD, CDD)],
            mobs[, .(Scenario = "Observed 2018-2025 average", month, HDD, CDD)])
mm <- melt(mm, id.vars = c("Scenario", "month"), variable.name = "var")
mm[, var := factor(var, labels = c("Heating degree-days (HDD14)", "Cooling degree-days (CDD21)"))]
mm[, Scenario := factor(Scenario, levels = names(cols))]
p2 <- ggplot(mm, aes(factor(month, labels = month.abb), value, fill = Scenario)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  facet_wrap(~var, ncol = 1, scales = "free_y") + scale_fill_manual(NULL, values = cols) +
  labs(x = NULL, y = "Degree-days in the month (national)",
       title = "Monthly degree-days: extreme vs standard vs observed average",
       subtitle = "Extreme year: much more heating in Jan, Feb and Nov; more cooling in Jun and Sep") + th
save(p2, "2_monthly_degree_days_2030.png", h = 7)

# (3) Cumulative degree-days through the year
cum <- rbind(n30[order(date), .(doy, HDD = cumsum(hdd), CDD = cumsum(cdd)), by = Scenario],
             band[, .(Scenario = "Observed 2018-2025 average", doy = doy30, HDD = cumsum(hdd), CDD = cumsum(cdd))])
cum <- melt(cum, id.vars = c("Scenario", "doy"), variable.name = "var")
cum[, var := factor(var, labels = c("Cumulative HDD", "Cumulative CDD"))]
p3 <- ggplot(cum, aes(doy, value, colour = factor(Scenario, levels = names(cols)))) +
  geom_line(linewidth = 0.9) + facet_wrap(~var, scales = "free_y") +
  scale_colour_manual(NULL, values = cols) + xmonth +
  labs(x = NULL, y = "Degree-days accumulated since 1 January",
       title = "Where in the year the two scenarios pull apart",
       subtitle = "Steeper slope = more heating / cooling need in that period") + th
save(p3, "3_cumulative_degree_days_2030.png")

# (4) Distribution of daily temperature
dens <- rbind(n30[, .(Scenario = as.character(Scenario), tavg)],
              obs[, .(Scenario = "Observed 2018-2025 average", tavg)])
p4 <- ggplot(dens, aes(tavg, colour = factor(Scenario, levels = names(cols)))) +
  geom_density(linewidth = 1) + geom_vline(xintercept = c(14, 21), linetype = "dashed") +
  scale_colour_manual(NULL, values = c(cols[1:2], "Observed 2018-2025 average" = "grey35")) +
  labs(x = "Daily mean temperature, °C", y = "Share of days (density)",
       title = "How often each temperature occurs",
       subtitle = "Smoothed share of days at each daily mean temperature. Grey line: all days of 2018-2025 pooled.\nThe extreme year has more cold days and more hot days, and far fewer mild days between 14 and 21 °C.") + th
save(p4, "4_temperature_distribution_2030.png", w = 9)

# (5) Extreme minus standard, by zone and month
zm <- merge(wx30, wt[, .(province, zone, w = weight)], by = "province")[
  , .(tavg = sum(w * tavg) / sum(w)), by = .(scenario, zone, date)][
  , .(tavg = mean(tavg)), by = .(scenario, zone, month = month(date))]
zm <- dcast(zm, zone + month ~ scenario, value.var = "tavg")[, diff := extreme - standard]
zm[, zone := factor(zone, levels = c("north", "interior", "med", "south"),
                    labels = c("North", "Interior", "Mediterranean", "South"))]
p5 <- ggplot(zm, aes(factor(month, labels = month.abb), zone, fill = diff)) +
  geom_tile(colour = "white") + geom_text(aes(label = sprintf("%+.1f", diff)), size = 3.6) +
  scale_fill_gradient2("Extreme minus\nstandard, °C", low = "#2166AC", mid = "white", high = "#B2182B") +
  labs(x = NULL, y = NULL, title = "Monthly mean temperature: extreme year minus standard year, by climate zone",
       subtitle = "Blue = extreme year colder, red = extreme year warmer") + th +
  theme(legend.position = "right")
save(p5, "5_temperature_difference_by_zone_month.png", h = 4)

# (6) Winter zoom: daily national HDD, 1 Jan to 31 Mar
wz <- n30[month <= 3]
p6 <- ggplot(wz, aes(as.Date(date), hdd, colour = Scenario)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = band[doy30 <= 90], aes(as.Date("2029-12-31") + doy30, hdd, colour = "Observed 2018-2025 average"),
            linewidth = 0.6) +
  annotate("text", x = as.Date("2030-02-14"), y = 9.6, label = "Cold spell 11-15 Feb\n(extreme peak hour: 14 Feb 20h)", size = 3.5) +
  scale_colour_manual(NULL, values = cols) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(x = NULL, y = "Daily national HDD (14 - mean temperature)",
       title = "Winter close-up: heating degree-days, January to March 2030") + th
save(p6, "6_winter_hdd_2030.png")

cat("HDD 99th pct threshold (2018-2025):", round(thr, 2), "\n")
print(annual[order(scenario)], digits = 4)
print(monthly_w, digits = 3)
print(spells)
print(top("hdd", 8))
print(top("cdd", 5))
print(peak_days[order(scenario, date)], digits = 3)
print(dcast(zfeb, zone ~ scenario, value.var = "HDD"), digits = 3)
