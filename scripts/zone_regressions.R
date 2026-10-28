# Zone regressions: peninsular hourly demand on HDD / CDD of four climate zones
#
# Instead of one national HDD / CDD with fixed weights, each zone's HDD / CDD
# gets its own coefficient, so the data decide how much each region's weather
# moves national demand. Zones group whole autonomous communities:
#   north     Galicia, Asturias, Cantabria, Pais Vasco
#   interior  Castilla y Leon, La Rioja, Navarra, Aragon, Madrid,
#             Castilla-La Mancha, Extremadura
#   med       Cataluna, Comunidad Valenciana, Murcia
#   south     Andalucia
#
# Weights reflect total demand: community share of peninsular demand (from the
# community file) times the province's share of small-consumer demand within
# its community. Zone HDD / CDD = weighted mean over the zone's provinces.
#
# Same fixed effects as the baseline M2 / M4: hour x month, hour x day type,
# year; SE clustered by week.
#
# Run from the repository root:  Rscript scripts/zone_regressions.R

source("scripts/prepare_data.R")

out_dir <- "output/zones"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(4)

# ---------------------------------------------------------------------------
# Provinces, communities and zones
# ---------------------------------------------------------------------------

community_of <- c(
  ALMERIA = "andalucia", CADIZ = "andalucia", CORDOBA = "andalucia", GRANADA = "andalucia",
  HUELVA = "andalucia", JAEN = "andalucia", MALAGA = "andalucia", SEVILLA = "andalucia",
  HUESCA = "aragon", TERUEL = "aragon", ZARAGOZA = "aragon",
  ASTURIAS = "asturias", CANTABRIA = "cantabria",
  ALBACETE = "castilla_la_mancha", `CIUDAD REAL` = "castilla_la_mancha", CUENCA = "castilla_la_mancha",
  GUADALAJARA = "castilla_la_mancha", TOLEDO = "castilla_la_mancha",
  AVILA = "castilla_y_leon", BURGOS = "castilla_y_leon", LEON = "castilla_y_leon",
  PALENCIA = "castilla_y_leon", SALAMANCA = "castilla_y_leon", SEGOVIA = "castilla_y_leon",
  SORIA = "castilla_y_leon", VALLADOLID = "castilla_y_leon", ZAMORA = "castilla_y_leon",
  BARCELONA = "cataluna", GERONA = "cataluna", LERIDA = "cataluna", TARRAGONA = "cataluna",
  ALICANTE = "comunidad_valenciana", CASTELLON = "comunidad_valenciana", VALENCIA = "comunidad_valenciana",
  BADAJOZ = "extremadura", CACERES = "extremadura",
  `LA CORUNA` = "galicia", LUGO = "galicia", ORENSE = "galicia", PONTEVEDRA = "galicia",
  `LA RIOJA` = "la_rioja", MADRID = "madrid", MURCIA = "murcia", NAVARRA = "navarra",
  ALAVA = "pais_vasco", GUIPUZCOA = "pais_vasco", VIZCAYA = "pais_vasco"
)
zone_of <- c(
  galicia = "north", asturias = "north", cantabria = "north", pais_vasco = "north",
  castilla_y_leon = "interior", la_rioja = "interior", navarra = "interior", aragon = "interior",
  madrid = "interior", castilla_la_mancha = "interior", extremadura = "interior",
  cataluna = "med", comunidad_valenciana = "med", murcia = "med",
  andalucia = "south"
)
zones <- c("north", "interior", "med", "south")

geo <- data.table(province = names(community_of), community = unname(community_of))
geo[, zone := zone_of[community]]
stopifnot(setequal(geo$province, unique(wx$province)))

# ---------------------------------------------------------------------------
# Check the grouping against the weather data: cluster provinces on the
# correlation of their daily temperature deviations from the monthly mean
# ---------------------------------------------------------------------------

tw <- dcast(wx[date >= sample_start & date <= sample_end], date ~ province, value.var = "tavg")
tdev <- as.matrix(tw[, -1])
tdev <- apply(tdev, 2, function(x) x - ave(x, format(tw$date, "%Y-%m")))
hc <- hclust(as.dist(1 - cor(tdev)), method = "average")
geo[, cluster := cutree(hc, k = 4)[province]]
fwrite(geo[order(zone, cluster, province)], file.path(out_dir, "zones_vs_clusters.csv"))
cat("Geographic zones vs data-driven clusters (provinces):\n")
print(table(zone = geo$zone, cluster = geo$cluster))

png(file.path(out_dir, "province_clusters.png"), width = 1800, height = 800, res = 150)
par(mar = c(1, 4, 2, 1))
plot(hc, labels = paste0(colnames(tdev), " (", geo[match(colnames(tdev), province), zone], ")"),
     main = "Provinces clustered on daily temperature deviations (1 - correlation)",
     xlab = "", sub = "", cex = 0.6, ylab = "1 - correlation")
rect.hclust(hc, k = 4, border = "#C2571A")
dev.off()

# ---------------------------------------------------------------------------
# Total-demand weights
# ---------------------------------------------------------------------------

comm <- fread(file.path(raw_dir, "hourly_demand_community_2015_2025.csv"),
              colClasses = list(character = "datetime"))
comm[, date := as.IDate(substr(datetime, 1, 10))]
comm <- comm[date >= sample_start & date <= sample_end]
comm_cols <- names(zone_of)
comm_share <- data.table(community = comm_cols,
                         c_share = sapply(comm_cols, function(k) sum(comm[[k]]) / sum(comm$peninsula)))

wt <- merge(geo, w[, .(province, small = w)], by = "province")
wt[, within := small / sum(small), by = community]
wt <- merge(wt, comm_share, by = "community")
wt[, weight := c_share * within]          # sums to 1 over the 47 provinces
wt[, zone_weight := sum(weight), by = zone]
fwrite(wt[order(zone, -weight)], file.path(out_dir, "province_weights_total_demand.csv"))
cat("\nZone shares of peninsular demand:\n")
print(unique(wt[, .(zone, zone_weight)]))

# Zone and national HDD / CDD by day
wz <- merge(wx[date >= sample_start & date <= sample_end], wt[, .(province, zone, weight)], by = "province")
zwx <- wz[, .(hdd = sum(weight * hdd) / sum(weight), cdd = sum(weight * cdd) / sum(weight)), by = .(date, zone)]
zwx_wide <- dcast(zwx, date ~ zone, value.var = c("hdd", "cdd"))
nat2 <- wz[, .(hdd_nat = sum(weight * hdd), cdd_nat = sum(weight * cdd)), by = date]

d <- merge(pen[, !c("hdd", "cdd", "tavg")], zwx_wide, by = "date")
d <- merge(d, nat2, by = "date")

# Zone deviations from the national value (for the test of the fixed weights)
for (z in zones) {
  d[, paste0("hdd_dev_", z) := get(paste0("hdd_", z)) - hdd_nat]
  d[, paste0("cdd_dev_", z) := get(paste0("cdd_", z)) - cdd_nat]
}

daily <- unique(d[, c("date", paste0("cdd_", zones), paste0("hdd_", zones)), with = FALSE])
daily[, ym := format(date, "%Y-%m")]
dm <- daily[, lapply(.SD, function(x) x - mean(x)), by = ym, .SDcols = c(paste0("cdd_", zones), paste0("hdd_", zones))]
cat("\nCorrelation of zone CDD, within month:\n")
print(round(cor(dm[, paste0("cdd_", zones), with = FALSE]), 2))

# ---------------------------------------------------------------------------
# Regressions (log demand, M2 / M4 fixed effects, SE clustered by week)
#   N2  national HDD / CDD, total-demand weights, one coefficient
#   Z2  four zones, one coefficient per zone
#   T2  national + three zone deviations: H0 deviations = 0 <=> fixed weights fine
#   N4, Z4, T4  the same with coefficients by hour of day
# ---------------------------------------------------------------------------

fe <- "| hour^month + hour^daytype + year"
zt <- function(v, z = zones) paste0(v, "_", z)
f <- function(rhs) as.formula(paste("ldemand ~", rhs, fe))

dev3 <- zones[-1]   # deviations sum to zero (weighted), so drop one zone
rhs_Z2 <- paste(c(zt("hdd"), zt("cdd")), collapse = " + ")
rhs_T2 <- paste(c("hdd_nat", "cdd_nat", zt("hdd_dev", dev3), zt("cdd_dev", dev3)), collapse = " + ")
rhs_Z4 <- paste(sprintf("i(hour, %s)", c(zt("hdd"), zt("cdd"))), collapse = " + ")
rhs_T4 <- paste(sprintf("i(hour, %s)", c("hdd_nat", "cdd_nat", zt("hdd_dev", dev3), zt("cdd_dev", dev3))),
                collapse = " + ")

m <- list(
  N2 = feols(f("hdd_nat + cdd_nat"), d, cluster = ~week),
  Z2 = feols(f(rhs_Z2), d, cluster = ~week),
  T2 = feols(f(rhs_T2), d, cluster = ~week),
  N4 = feols(f("i(hour, hdd_nat) + i(hour, cdd_nat)"), d, cluster = ~week),
  Z4 = feols(f(rhs_Z4), d, cluster = ~week),
  T4 = feols(f(rhs_T4), d, cluster = ~week)
)

dict <- c(ldemand = "log demand", hdd_nat = "HDD national", cdd_nat = "CDD national",
          setNames(paste("HDD", zones), zt("hdd")), setNames(paste("CDD", zones), zt("cdd")),
          setNames(paste("HDD dev.", zones), zt("hdd_dev")), setNames(paste("CDD dev.", zones), zt("cdd_dev")))

wald_dev <- function(model) {
  wd <- wald(model, keep = "_dev_", print = FALSE)
  data.table(stat = wd$stat, p = wd$p, df1 = wd$df1, df2 = wd$df2)
}
tests <- rbind(cbind(model = "T2 (pooled hours)", wald_dev(m$T2)),
               cbind(model = "T4 (by hour)", wald_dev(m$T4)))

sink(file.path(out_dir, "zone_tables.txt"))
cat("== Peninsular demand (log MW), hourly, 2021-06 to 2025-12 ==\n")
print(etable(m[c("N2", "Z2", "T2")], dict = dict, digits = 5, fitstat = ~ n + r2 + wr2))
cat("\nWithin R2 by hour-interacted model: ",
    sprintf("N4 %.4f | Z4 %.4f | T4 %.4f\n", fitstat(m$N4, "wr2")[[1]], fitstat(m$Z4, "wr2")[[1]],
            fitstat(m$T4, "wr2")[[1]]))
cat("\nWald test that zone deviations are jointly zero (fixed national weights are enough):\n")
print(tests)
sink()
fwrite(tests, file.path(out_dir, "weight_tests.csv"))

# ---------------------------------------------------------------------------
# Implied effects
# ---------------------------------------------------------------------------

b <- coef(m$Z2)
zone_tab <- unique(wt[, .(zone, zone_weight)])
zone_tab[, `:=`(
  cdd_pct = 100 * b[paste0("cdd_", zone)], cdd_se = 100 * se(m$Z2)[paste0("cdd_", zone)],
  hdd_pct = 100 * b[paste0("hdd_", zone)], hdd_se = 100 * se(m$Z2)[paste0("hdd_", zone)]
)]
# Per unit of demand share: the national effect if weights were exactly proportional to demand
zone_tab[, `:=`(cdd_per_share = cdd_pct / zone_weight, hdd_per_share = hdd_pct / zone_weight)]
zone_tab[, cdd_mw := cdd_pct / 100 * mean(d$demand)]
zone_tab <- rbind(zone_tab,
                  data.table(zone = "sum of zones", zone_weight = 1,
                             cdd_pct = sum(zone_tab$cdd_pct), hdd_pct = sum(zone_tab$hdd_pct),
                             cdd_mw = sum(zone_tab$cdd_mw)),
                  data.table(zone = "national (N2)", zone_weight = 1,
                             cdd_pct = 100 * coef(m$N2)["cdd_nat"], hdd_pct = 100 * coef(m$N2)["hdd_nat"],
                             cdd_se = 100 * se(m$N2)["cdd_nat"], hdd_se = 100 * se(m$N2)["hdd_nat"],
                             cdd_mw = coef(m$N2)["cdd_nat"] * mean(d$demand)),
                  fill = TRUE)
fwrite(zone_tab, file.path(out_dir, "zone_effects_Z2.csv"))

# Predicted temperature effect on the hottest days: zone model vs national model
daily_pred <- unique(d[, c("date", zt("cdd"), zt("hdd"), "cdd_nat", "hdd_nat"), with = FALSE])
daily_pred[, `:=`(
  eff_zone = exp(as.matrix(.SD[, c(zt("hdd"), zt("cdd")), with = FALSE]) %*% b[c(zt("hdd"), zt("cdd"))])[, 1] - 1,
  eff_nat = exp(hdd_nat * coef(m$N2)["hdd_nat"] + cdd_nat * coef(m$N2)["cdd_nat"]) - 1
)]
hot <- daily_pred[order(-cdd_nat)][1:20]
fwrite(daily_pred[order(-cdd_nat)], file.path(out_dir, "daily_predicted_effects.csv"))

# Hour-by-hour CDD effects by zone (Z4)
ct <- as.data.table(coeftable(m$Z4), keep.rownames = "term")
setnames(ct, c("term", "est", "se", "t", "p"))
ct[, `:=`(hour = as.integer(sub("^hour::([0-9]+):.*", "\\1", term)), var = sub("^hour::[0-9]+:", "", term))]
ct[, `:=`(pct = 100 * est, lo = 100 * (est - 1.96 * se), hi = 100 * (est + 1.96 * se))]
fwrite(ct[, .(var, hour, pct, lo, hi)], file.path(out_dir, "hourly_zone_effects_Z4.csv"))

cols <- c(north = "#5B8DB8", interior = "#1F4E79", med = "#E8A24A", south = "#C2571A")
png(file.path(out_dir, "hourly_zone_effects_Z4.png"), width = 1600, height = 700, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4.5, 2.5, 1))
for (v in c("cdd", "hdd")) {
  s <- ct[startsWith(var, v)]
  plot(NA, xlim = c(0, 23), ylim = range(c(0, s$lo, s$hi)), xaxt = "n",
       xlab = "Hour of day (local time)", ylab = "% change in national demand per zone degree-day",
       main = if (v == "cdd") "Cooling: CDD21 by zone" else "Heating: HDD14 by zone")
  axis(1, at = seq(0, 23, 3)); abline(h = 0, col = "grey60")
  for (z in zones) {
    x <- s[var == paste0(v, "_", z)][order(hour)]
    polygon(c(x$hour, rev(x$hour)), c(x$lo, rev(x$hi)), col = adjustcolor(cols[z], 0.15), border = NA)
    lines(x$hour, x$pct, col = cols[z], lwd = 2)
  }
  if (v == "cdd") legend("topleft", zones, col = cols, lwd = 2, bty = "n", cex = 0.85)
}
dev.off()

# Robustness: zones from the data-driven clusters instead of geography
wc <- merge(wx[date >= sample_start & date <= sample_end],
            merge(wt[, .(province, weight)], geo[, .(province, cluster)], by = "province"), by = "province")
cwx <- dcast(wc[, .(hdd = sum(weight * hdd) / sum(weight), cdd = sum(weight * cdd) / sum(weight)),
                by = .(date, cluster)], date ~ cluster, value.var = c("hdd", "cdd"))
dc <- merge(pen[, !c("hdd", "cdd", "tavg")], cwx, by = "date")
cl_vars <- grep("^(hdd|cdd)_[0-9]$", names(dc), value = TRUE)
m$Z2c <- feols(f(paste(cl_vars, collapse = " + ")), dc, cluster = ~week)
cl_share <- unique(merge(wt[, .(province, weight)], geo[, .(province, cluster)])[, .(share = sum(weight)), by = cluster])

sink(file.path(out_dir, "zone_tables.txt"), append = TRUE)
cat("\nRobustness Z2c: zones = data-driven clusters (demand shares below)\n")
print(cl_share[order(cluster)])
print(etable(m$Z2c, digits = 5, fitstat = ~ n + wr2))
cat("Sum of CDD coefficients:", sum(coef(m$Z2c)[grep("^cdd", names(coef(m$Z2c)))]),
    " HDD:", sum(coef(m$Z2c)[grep("^hdd", names(coef(m$Z2c)))]), "\n")
cat("\nZone effects (Z2), % per zone degree-day; per_share = effect / zone demand share:\n")
print(zone_tab, digits = 3)
cat("\n20 hottest days (national CDD): predicted temperature effect, zone vs national model\n")
print(hot[, .(date, cdd_nat, eff_zone = round(100 * eff_zone, 1), eff_nat = round(100 * eff_nat, 1))])
sink()

cat("Done. Results in", out_dir, "\n")
