"""Artificial degree-day scenarios on 2025 and the M4 demand they imply.

Base: actual 2025 provincial HDD14/CDD21 (raw_data/HDD14_CDD21.xlsx), 2025 calendar,
2025 level (year effect). Scenarios multiply degree days:
    s1_plus10: winter (Dec-Feb) HDD x 1.10 and summer (Jun-Aug) CDD x 1.10
    s2_plus20: winter (Dec-Feb) HDD x 1.20 and summer (Jun-Aug) CDD x 1.20
Other months keep their actual 2025 degree days. Variant '_allyear' scales HDD and
CDD on every day (reported in the summary for comparison only).
Scaling each province by k scales the national (weighted) value by k as well.

Demand: M4 on the 2025 calendar. log D = calendar terms + b_H(h)*HDD + b_C(h)*CDD;
calendar terms come from output/energeia_2025cal/hourly_2025cal_standard_M4.csv
(scripts/project_2025_calendar.py) by swapping the weather term.
Run from the repository root.
"""
import datetime as dt
import unicodedata
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import openpyxl
import pandas as pd

OUT = Path("output/degree_day_scaling")
OUT.mkdir(parents=True, exist_ok=True)
SCEN = {"base": 1.0, "s1_plus10": 1.10, "s2_plus20": 1.20}
w = pd.read_csv("output/baseline/province_weights.csv").set_index("province").w


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
        parts.append(d[["date", "province", "hdd", "cdd"]])
    return pd.concat(parts)


prov = provinces("raw_data/HDD14_CDD21.xlsx", 2025)
assert prov.date.nunique() == 365 and prov.province.nunique() == 47
std = provinces("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx")
std["date"] = pd.to_datetime("2025-" + std.date.dt.strftime("%m-%d"))


def national(p):
    x = p.assign(w=p.province.map(w))
    return x.assign(hdd=x.hdd * x.w, cdd=x.cdd * x.w).groupby("date")[["hdd", "cdd"]].sum()


nat0 = national(prov)
nat_std = national(std)
month = nat0.index.month
winter, summer = month.isin([12, 1, 2]), month.isin([6, 7, 8])

# Degree-day scenarios (national, daily)
dd = pd.DataFrame(index=nat0.index)
for s, k in SCEN.items():
    dd[f"hdd_{s}"] = np.where(winter, nat0.hdd * k, nat0.hdd)
    dd[f"cdd_{s}"] = np.where(summer, nat0.cdd * k, nat0.cdd)
for s, k in list(SCEN.items())[1:]:
    dd[f"hdd_{s}_allyear"] = nat0.hdd * k
    dd[f"cdd_{s}_allyear"] = nat0.cdd * k

# Demand
eff = pd.read_csv("output/baseline/hourly_effects_M4.csv")
eff = eff[eff["sample"].str.startswith("Pen")]
bH = eff[eff["var"] == "hdd"].set_index("hour").pct / 100
bC = eff[eff["var"] == "cdd"].set_index("hour").pct / 100
ref = pd.read_csv("output/energeia_2025cal/hourly_2025cal_standard_M4.csv", parse_dates=["date"])
h = ref.hour.values
H_ref, C_ref = nat_std.hdd.reindex(ref.date).values, nat_std.cdd.reindex(ref.date).values


def demand(hdd_daily, cdd_daily):
    Hd, Cd = hdd_daily.reindex(ref.date).values, cdd_daily.reindex(ref.date).values
    return ref.mw.values * np.exp(bH[h].values * (Hd - H_ref) + bC[h].values * (Cd - C_ref))


actual = pd.read_excel("output/energeia/energeia_demand_2025_actual.xlsx", header=None,
                       skiprows=5, usecols=[2]).iloc[:8760, 0].astype(float).values
D = pd.DataFrame({"date": ref.date, "hour": h, "actual_2025": actual})
for s in SCEN:
    D[s] = demand(dd[f"hdd_{s}"], dd[f"cdd_{s}"])
for s in list(SCEN)[1:]:
    D[f"{s}_allyear"] = demand(dd[f"hdd_{s}_allyear"], dd[f"cdd_{s}_allyear"])

# Energeia files (main version)
tpl = "raw_data/energeia_demand_template.xlsx"
for s in SCEN:
    wb = openpyxl.load_workbook(tpl)
    for n in [n for n in wb.sheetnames if n != "Template_demand"]:
        del wb[n]
    ws = wb["Template_demand"]
    ws["E1"] = 2025
    ws["D5"] = None
    ws.cell(6, 2).value = dt.datetime(2025, 1, 1)
    for i, v in enumerate(D[s].values):
        ws.cell(6 + i, 3).value = float(round(v, 1))
        ws.cell(6 + i, 4).value = None
    wb.save(OUT / f"energeia_demand_2025_dd_{s}_M4.xlsx")

# Summaries
cols = ["actual_2025", *SCEN, *[f"{s}_allyear" for s in list(SCEN)[1:]]]
D["month"] = D.date.dt.month
mon = D.groupby("month")[cols].sum() / 1e6
mon.loc["Year"] = mon.sum()
mdd = dd.groupby(dd.index.month).sum()
mdd.loc["Year"] = mdd.sum()
summ = []
for c in cols:
    i = D[c].idxmax()
    summ.append({"series": c, "TWh": D[c].sum() / 1e6,
                 "change_vs_base_TWh": (D[c].sum() - D["base"].sum()) / 1e6,
                 "change_vs_base_pct": 100 * (D[c].sum() / D["base"].sum() - 1),
                 "summer_JJA_TWh": D.loc[D.month.isin([6, 7, 8]), c].sum() / 1e6,
                 "winter_DJF_TWh": D.loc[D.month.isin([12, 1, 2]), c].sum() / 1e6,
                 "peak_MW": D[c].max(), "peak_hour": f"{D.date[i]:%Y-%m-%d} {D.hour[i]:02d}h"})
summ = pd.DataFrame(summ)

prov_out = prov.sort_values(["province", "date"]).rename(columns={"hdd": "hdd_base", "cdd": "cdd_base"})
for s, k in list(SCEN.items())[1:]:
    m = prov_out.date.dt.month
    prov_out[f"hdd_{s}"] = np.where(m.isin([12, 1, 2]), prov_out.hdd_base * k, prov_out.hdd_base)
    prov_out[f"cdd_{s}"] = np.where(m.isin([6, 7, 8]), prov_out.cdd_base * k, prov_out.cdd_base)
readme = pd.DataFrame({"note": [
    "Artificial degree-day scenarios built on ACTUAL 2025 weather (observed provincial HDD14/CDD21).",
    "base = actual 2025 degree days; s1_plus10 = winter (Dec-Feb) HDD and summer (Jun-Aug) CDD x 1.10; s2_plus20 = same x 1.20. Other months unchanged.",
    "*_allyear = HDD and CDD scaled on every day of the year. For comparison only.",
    "National = small-consumer-weighted mean of the 47 provinces (same as M4). Scaling provinces by k scales the national value by k.",
    "Demand: M4, 2025 calendar (weekdays, holidays) and 2025 level. base = M4 fitted with actual 2025 weather.",
    "Demand in MW (= MWh per hour) hourly, TWh in summaries.",
]})
with pd.ExcelWriter(OUT / "degree_days_and_demand_2025_scaled.xlsx") as xw:
    readme.to_excel(xw, sheet_name="README", index=False)
    summ.round(3).to_excel(xw, sheet_name="Summary", index=False)
    mon.round(3).reset_index().to_excel(xw, sheet_name="Monthly_demand_TWh", index=False)
    mdd.round(1).reset_index().rename(columns={"date": "month"}).to_excel(xw, sheet_name="Monthly_degree_days", index=False)
    dd.round(3).reset_index().to_excel(xw, sheet_name="National_daily_DD", index=False)
    prov_out.round(3).to_excel(xw, sheet_name="Province_daily_DD", index=False)
    D.drop(columns="month").round(1).to_excel(xw, sheet_name="Hourly_demand_MW", index=False)

# Chart
fig, ax = plt.subplots(2, 1, figsize=(12, 8))
x = dd.index
col = {"base": "black", "s1_plus10": "#fd8d3c", "s2_plus20": "#bd0026"}
lab = {"base": "Actual 2025 degree days", "s1_plus10": "+10%", "s2_plus20": "+20%"}
for s in SCEN:
    ax[0].plot(x, dd[f"cdd_{s}"].rolling(7, center=True, min_periods=1).mean(), color=col[s], lw=1.8,
               label=f"CDD, {lab[s]} ({dd[f'cdd_{s}'].sum():.0f}/yr)")
    ax[0].plot(x, dd[f"hdd_{s}"].rolling(7, center=True, min_periods=1).mean(), color=col[s], lw=1.8, ls="--",
               label=f"HDD, {lab[s]} ({dd[f'hdd_{s}'].sum():.0f}/yr)")
ax[0].set_title("National degree days, 7-day average (solid = CDD base 21 °C, dashed = HDD base 14 °C)")
ax[0].set_ylabel("Degree days per day")
ax[0].set_ylim(0, 12)
ax[0].legend(ncol=3, fontsize=8.5, loc="upper center")
ax[0].grid(alpha=0.3)
ax[0].xaxis.set_major_formatter(matplotlib.dates.DateFormatter("%b"))
mm = mon.drop("Year")
bw = 0.38
for j, s in enumerate(["s1_plus10", "s2_plus20"]):
    ax[1].bar(mm.index + (j - 0.5) * bw, (mm[s] - mm["base"]) * 1e3, width=bw, color=col[s],
              label=f"{lab[s]}: {(mon.loc['Year', s] - mon.loc['Year', 'base']):+.2f} TWh/yr "
                    f"({100 * (mon.loc['Year', s] / mon.loc['Year', 'base'] - 1):+.1f}%)")
ax[1].set_title("Change in monthly peninsular demand vs base (M4, 2025 calendar and level; winter = Dec-Feb, summer = Jun-Aug)")
ax[1].set_ylabel("GWh")
ax[1].set_xticks(range(1, 13), ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"])
ax[1].legend(fontsize=9)
ax[1].grid(alpha=0.3, axis="y")
fig.tight_layout()
fig.savefig(OUT / "degree_day_scaling_2025.png", dpi=150, facecolor="white")

pd.set_option("display.width", 220)
print(summ.round(2).to_string())
print(mon.round(2).to_string())
print(mdd.round(0).to_string())
