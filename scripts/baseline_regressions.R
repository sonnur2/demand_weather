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

source("scripts/prepare_data.R")

out_dir <- "output/baseline"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(w[order(-w)], file.path(out_dir, "province_weights.csv"))

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
