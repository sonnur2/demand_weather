# Zone definitions and total-demand province weights, shared by the zone
# regressions and the 2030 projections. Needs prepare_data.R to be sourced first.
#
#   geo   province -> community -> zone
#   wt    province weights: community share of peninsular demand times the
#         province's share of small-consumer demand within its community
#   zone_weather(x)  daily province HDD / CDD -> zone and national HDD / CDD

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

# Daily province weather (date, province, hdd, cdd) -> one row per date with
# hdd_<zone>, cdd_<zone> (weighted means within zone) and hdd_nat, cdd_nat
zone_weather <- function(x) {
  x <- merge(x, wt[, .(province, zone, weight)], by = "province")
  zw <- x[, .(hdd = sum(weight * hdd) / sum(weight), cdd = sum(weight * cdd) / sum(weight)), by = .(date, zone)]
  out <- dcast(zw, date ~ zone, value.var = c("hdd", "cdd"))
  merge(out, x[, .(hdd_nat = sum(weight * hdd), cdd_nat = sum(weight * cdd)), by = date], by = "date")
}
