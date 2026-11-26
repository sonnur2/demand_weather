"""National HDD/CDD: actual 2025 vs 2030 standard and extreme, under several definitions.

Plots daily national HDD and CDD (small-consumer weighted, as used in M4) and
tabulates the same comparison for other aggregations and thresholds.
Output: output/compare_2025_actual/ (png + xlsx). Run from the repository root.
"""
import unicodedata
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT = Path("output/compare_2025_actual")
OUT.mkdir(parents=True, exist_ok=True)


def norm(s):
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().upper().strip()


def provinces(path, year=None):
    parts = []
    for s, d in pd.read_excel(path, sheet_name=None).items():
        if not s[:2].isdigit():
            continue
        d = d.iloc[:, :6].copy()
        d.columns = ["date", "tmin", "tmax", "tavg", "hdd", "cdd"]
        d["date"] = pd.to_datetime(d.date)
        if year:
            d = d[d.date.dt.year == year]
        d["province"] = norm(s[3:])
        d["md"] = d.date.dt.strftime("%m-%d")
        parts.append(d.drop(columns="date"))
    return pd.concat(parts)


P = pd.concat([
    provinces("raw_data/HDD14_CDD21.xlsx", 2025).assign(year="Actual 2025"),
    provinces("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx").assign(year="Standard (2030 from 2024)"),
    provinces("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_1991.xlsx").assign(year="Extreme (2030 from 1991)"),
])
P["date"] = pd.to_datetime("2025-" + P.md)
ws = pd.read_csv("output/baseline/province_weights.csv").set_index("province").w
wt = pd.read_csv("output/zones/province_weights_total_demand.csv").set_index("province")
P["w_small"] = P.province.map(ws)
P["w_total"] = P.province.map(wt.weight)
P["w_equal"] = 1 / 47
P["zone"] = P.province.map(wt.zone)
# Alternative thresholds, computed from daily mean temperature
for b in (18, 21, 24):
    P[f"cdd{b}_tavg"] = (P.tavg - b).clip(lower=0)
for b in (14, 16, 18):
    P[f"hdd{b}_tavg"] = (b - P.tavg).clip(lower=0)


def nat(wcol, cols=("hdd", "cdd")):
    x = P[["year", "date", wcol, *cols]].copy()
    for c in cols:
        x[c] = x[c] * x[wcol]
    g = x.groupby(["year", "date"])[list(cols)].sum()
    return g.div(x.groupby(["year", "date"])[wcol].sum(), axis=0).reset_index()


# --- Chart: M4 definition (small-consumer weights) -------------------------
d = nat("w_small")
colors = {"Actual 2025": "black", "Standard (2030 from 2024)": "#1f77b4", "Extreme (2030 from 1991)": "#d62728"}
fig, ax = plt.subplots(2, 1, figsize=(12, 7.5), sharex=True)
for var, a, title in [("cdd", ax[0], "Cooling degree days (CDD, base 21 °C)"),
                      ("hdd", ax[1], "Heating degree days (HDD, base 14 °C)")]:
    for y, c in colors.items():
        s = d[d.year == y].set_index("date")[var]
        a.plot(s.index, s, color=c, lw=0.6, alpha=0.30)
        a.plot(s.index, s.rolling(7, center=True, min_periods=1).mean(), color=c, lw=2,
               label=f"{y}: {s.sum():.0f} per year")
    a.set_title(title + ", national, small-consumer weighted")
    a.set_ylabel("Degree days per day")
    a.grid(alpha=0.3)
    a.legend(loc="upper left" if var == "cdd" else "upper center", fontsize=9,
             title="Thick line = 7-day average, thin line = daily value", title_fontsize=8.5)
ax[1].xaxis.set_major_formatter(matplotlib.dates.DateFormatter("%b"))
ax[1].xaxis.set_major_locator(matplotlib.dates.MonthLocator())
fig.tight_layout()
fig.savefig(OUT / "national_hdd_cdd_actual2025_standard_extreme.png", dpi=150, facecolor="white")

# --- Versions table ----------------------------------------------------------
periods = {"Year": range(1, 13), "June": [6], "July-August": [7, 8], "September": [9],
           "Jan-Mar": [1, 2, 3], "November-December": [11, 12]}
versions = [
    ("Small-consumer weighted (used in M1-M4)", "w_small", "hdd", "cdd"),
    ("Total-demand weighted (N2/N4)", "w_total", "hdd", "cdd"),
    ("Unweighted mean of 47 provinces", "w_equal", "hdd", "cdd"),
    ("From daily mean temp., base 18 (CDD) / 16 (HDD)", "w_small", "hdd16_tavg", "cdd18_tavg"),
    ("From daily mean temp., base 21 (CDD) / 14 (HDD)", "w_small", "hdd14_tavg", "cdd21_tavg"),
    ("From daily mean temp., base 24 (CDD) / 18 (HDD)", "w_small", "hdd18_tavg", "cdd24_tavg"),
]
rows = []
for name, wcol, h, c in versions:
    x = nat(wcol, (h, c))
    x["month"] = x.date.dt.month
    for var, col in [("CDD", c), ("HDD", h)]:
        for per, ms in periods.items():
            if (var == "CDD" and per in ("Jan-Mar", "November-December")) or (var == "HDD" and per in ("June", "July-August", "September")):
                continue
            t = x[x.month.isin(ms)].groupby("year")[col].sum()
            rows.append({"version": name, "variable": var, "period": per, "actual_2025": t["Actual 2025"],
                         "standard": t["Standard (2030 from 2024)"], "extreme": t["Extreme (2030 from 1991)"]})
# Zones (small-consumer weights within zone)
for z in ["north", "interior", "med", "south"]:
    sub = P[P.zone == z]
    for var in ["cdd", "hdd"]:
        x = sub.assign(v=sub[var] * sub.w_small).groupby(["year", "date"]).v.sum() / sub.groupby(["year", "date"]).w_small.sum()
        x = x.reset_index(name="v")
        x["month"] = x.date.dt.month
        for per, ms in periods.items():
            if (var == "cdd" and per in ("Jan-Mar", "November-December")) or (var == "hdd" and per in ("June", "July-August", "September")):
                continue
            t = x[x.month.isin(ms)].groupby("year").v.sum()
            rows.append({"version": f"Zone {z} only", "variable": var.upper(), "period": per, "actual_2025": t["Actual 2025"],
                         "standard": t["Standard (2030 from 2024)"], "extreme": t["Extreme (2030 from 1991)"]})
T = pd.DataFrame(rows)
T["standard_minus_actual"] = T.standard - T.actual_2025
T["extreme_minus_actual"] = T.extreme - T.actual_2025
T = T.round(1)
daily = d.pivot(index="date", columns="year", values=["hdd", "cdd"])
daily.columns = [f"{v}_{y}" for v, y in daily.columns]
with pd.ExcelWriter(OUT / "national_degree_days_versions.xlsx") as xw:
    pd.DataFrame({"note": [
        "National HDD/CDD = weighted average over the 47 peninsular provinces of each province's daily HDD/CDD.",
        "M4 uses small-consumer weights: each province's share of household/small-business electricity demand, 2021-06 to 2025-12 (Madrid 13.2%, Barcelona 11.6%).",
        "Province HDD14/CDD21 come from the weather files (likely computed from sub-daily temperatures). Versions 'from daily mean temp.' recompute degree days as max(Tavg - base, 0) and max(base - Tavg, 0).",
        "standard_minus_actual > 0 means the 2024-based 2030 year is hotter (CDD) or colder (HDD) than actual 2025.",
    ]}).to_excel(xw, sheet_name="README", index=False)
    T.to_excel(xw, sheet_name="Versions", index=False)
    daily.round(2).reset_index().to_excel(xw, sheet_name="Daily_M4_definition", index=False)
pd.set_option("display.width", 250)
print(T.to_string())
