"""Degree-day scenarios on 2025 (winter HDD and summer CDD only) and M4 demand.

Base: actual 2025 provincial HDD14/CDD21 (raw_data/HDD14_CDD21.xlsx), 2025 calendar
(weekdays, national holidays), 2025 level (year effect). Scenarios change degree days
only in literal winter (Dec, Jan, Feb: HDD) and summer (Jun, Jul, Aug: CDD):
    10_pp : winter HDD -10%, summer CDD +10%
    20_pp : winter HDD -20%, summer CDD +20%
    lit   : winter HDD  -8%, summer CDD +19%   (literature-based)
Other months keep their actual 2025 degree days. Scaling each province by k scales the
national (small-consumer-weighted) value by k.

Demand: M4. log D = calendar terms + b_H(h)*HDD + b_C(h)*CDD; the calendar terms come
from output/energeia_2025cal/hourly_2025cal_standard_M4.csv (scripts/project_2025_calendar.py)
by swapping the weather term. Run from the repository root.
"""
import datetime as dt
import unicodedata
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import openpyxl
import pandas as pd

OUT = Path("output/degree_day_scenarios_2025")
OUT.mkdir(parents=True, exist_ok=True)
# name: (winter HDD factor, summer CDD factor, label)
SCEN = {"base": (1.00, 1.00, "Base: actual 2025 degree days"),
        "10_pp": (0.90, 1.10, "10_pp: HDD -10%, CDD +10%"),
        "20_pp": (0.80, 1.20, "20_pp: HDD -20%, CDD +20%"),
        "lit": (0.92, 1.19, "lit: HDD -8%, CDD +19%")}
COL = {"base": "black", "10_pp": "#fd8d3c", "20_pp": "#bd0026", "lit": "#2171b5"}
WINTER, SUMMER = [12, 1, 2], [6, 7, 8]
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


def national(p):
    x = p.assign(w=p.province.map(w))
    return x.assign(hdd=x.hdd * x.w, cdd=x.cdd * x.w).groupby("date")[["hdd", "cdd"]].sum()


prov = provinces("raw_data/HDD14_CDD21.xlsx", 2025)
assert prov.date.nunique() == 365 and prov.province.nunique() == 47
std = provinces("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx")
std["date"] = pd.to_datetime("2025-" + std.date.dt.strftime("%m-%d"))
nat0, nat_std = national(prov), national(std)
win, summ = nat0.index.month.isin(WINTER), nat0.index.month.isin(SUMMER)

dd = pd.DataFrame(index=nat0.index)
for s, (kh, kc, _) in SCEN.items():
    dd[f"hdd_{s}"] = np.where(win, nat0.hdd * kh, nat0.hdd)
    dd[f"cdd_{s}"] = np.where(summ, nat0.cdd * kc, nat0.cdd)

# Demand
eff = pd.read_csv("output/baseline/hourly_effects_M4.csv")
eff = eff[eff["sample"].str.startswith("Pen")]
bH = eff[eff["var"] == "hdd"].set_index("hour").pct / 100
bC = eff[eff["var"] == "cdd"].set_index("hour").pct / 100
ref = pd.read_csv("output/energeia_2025cal/hourly_2025cal_standard_M4.csv", parse_dates=["date"])
h = ref.hour.values
H_ref, C_ref = nat_std.hdd.reindex(ref.date).values, nat_std.cdd.reindex(ref.date).values
actual = pd.read_excel("output/energeia/energeia_demand_2025_actual.xlsx", header=None,
                       skiprows=5, usecols=[2]).iloc[:8760, 0].astype(float).values
D = pd.DataFrame({"date": ref.date, "hour": h, "actual_2025": actual})
for s in SCEN:
    Hd = dd[f"hdd_{s}"].reindex(ref.date).values
    Cd = dd[f"cdd_{s}"].reindex(ref.date).values
    D[s] = ref.mw.values * np.exp(bH[h].values * (Hd - H_ref) + bC[h].values * (Cd - C_ref))

# Energeia files
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
    wb.save(OUT / f"energeia_demand_2025_{s}_M4.xlsx")

# Summaries
D["month"] = D.date.dt.month
mons = D.month.values
cols = ["actual_2025", *SCEN]
rows = []
for c in cols:
    i = D[c].idxmax()
    js = D[D.month.isin(SUMMER)][c].idxmax()
    rows.append({"series": c, "TWh": D[c].sum() / 1e6,
                 "change_vs_base_TWh": (D[c].sum() - D["base"].sum()) / 1e6,
                 "change_vs_base_pct": 100 * (D[c].sum() / D["base"].sum() - 1),
                 "winter_DJF_TWh": D.loc[D.month.isin(WINTER), c].sum() / 1e6,
                 "summer_JJA_TWh": D.loc[D.month.isin(SUMMER), c].sum() / 1e6,
                 "annual_peak_MW": D[c].max(), "annual_peak_hour": f"{D.date[i]:%Y-%m-%d} {D.hour[i]:02d}h",
                 "summer_peak_MW": D[c][js], "summer_peak_hour": f"{D.date[js]:%Y-%m-%d} {D.hour[js]:02d}h",
                 "winter_HDD_DJF": dd.loc[win, f"hdd_{c}"].sum() if c in SCEN else np.nan,
                 "summer_CDD_JJA": dd.loc[summ, f"cdd_{c}"].sum() if c in SCEN else np.nan,
                 "annual_HDD": dd[f"hdd_{c}"].sum() if c in SCEN else np.nan,
                 "annual_CDD": dd[f"cdd_{c}"].sum() if c in SCEN else np.nan})
S = pd.DataFrame(rows)
mon = D.groupby("month")[cols].sum() / 1e6
mon.loc["Year"] = mon.sum()
mdd = dd.groupby(dd.index.month).sum()
mdd.loc["Year"] = mdd.sum()
prov_out = prov.sort_values(["province", "date"]).rename(columns={"hdd": "hdd_base", "cdd": "cdd_base"})
pm = prov_out.date.dt.month
for s, (kh, kc, _) in list(SCEN.items())[1:]:
    prov_out[f"hdd_{s}"] = np.where(pm.isin(WINTER), prov_out.hdd_base * kh, prov_out.hdd_base)
    prov_out[f"cdd_{s}"] = np.where(pm.isin(SUMMER), prov_out.cdd_base * kc, prov_out.cdd_base)
readme = pd.DataFrame({"note": [
    "Degree-day scenarios on 2025. Base = actual 2025 provincial HDD14/CDD21 (observed).",
    "Only literal winter (Dec-Feb) HDD and summer (Jun-Aug) CDD are changed; other months keep actual 2025 values.",
    "10_pp: HDD x0.90, CDD x1.10. 20_pp: HDD x0.80, CDD x1.20. lit: HDD x0.92, CDD x1.19 (literature-based).",
    "National = small-consumer-weighted mean of the 47 provinces (same as M4).",
    "Demand: M4 with 2025 calendar (weekdays, national holidays) and 2025 level (year effect). base = M4 fitted with actual 2025 weather.",
    "Demand hourly in MW (= MWh per hour); TWh in summaries. Energeia files: energeia_demand_2025_<scenario>_M4.xlsx.",
]})
with pd.ExcelWriter(OUT / "degree_day_scenarios_2025.xlsx") as xw:
    readme.to_excel(xw, sheet_name="README", index=False)
    S.round(3).to_excel(xw, sheet_name="Summary", index=False)
    mon.round(3).reset_index().to_excel(xw, sheet_name="Monthly_demand_TWh", index=False)
    mdd.round(1).reset_index().rename(columns={"date": "month"}).to_excel(xw, sheet_name="Monthly_degree_days", index=False)
    dd.round(3).reset_index().to_excel(xw, sheet_name="National_daily_DD", index=False)
    prov_out.to_excel(xw, sheet_name="Province_daily_DD", index=False)
    D.drop(columns="month").to_excel(xw, sheet_name="Hourly_demand_MW", index=False)

# --- Plot 1: degree days ----------------------------------------------------
fig, ax = plt.subplots(2, 1, figsize=(12, 8.5), sharex=True)
for var, a, title, period in [("hdd", ax[0], "Heating degree days (HDD, base 14 °C): changed only in Dec–Feb", WINTER),
                              ("cdd", ax[1], "Cooling degree days (CDD, base 21 °C): changed only in Jun–Aug", SUMMER)]:
    for s, (_, _, lab) in SCEN.items():
        y = dd[f"{var}_{s}"].rolling(7, center=True, min_periods=1).mean()
        tot = dd.loc[win if var == "hdd" else summ, f"{var}_{s}"].sum()
        a.plot(dd.index, y, color=COL[s], lw=2.2 if s == "base" else 1.6,
               label=f"{lab} ({'Dec–Feb' if var == 'hdd' else 'Jun–Aug'} total {tot:.0f})")
    a.set_title(title + " — national, 7-day average")
    a.set_ylabel("Degree days per day")
    a.grid(alpha=0.3)
    a.legend(fontsize=9, loc="upper center")
    a.set_ylim(0, 11)
ax[1].xaxis.set_major_locator(mdates.MonthLocator())
ax[1].xaxis.set_major_formatter(mdates.DateFormatter("%b"))
fig.tight_layout()
fig.savefig(OUT / "1_degree_day_scenarios_2025.png", dpi=150, facecolor="white")
plt.close(fig)

# --- Plot 2: monthly demand change vs base ------------------------------------
mm = mon.drop("Year")
fig, ax = plt.subplots(figsize=(12, 5.5))
bw = 0.27
for j, s in enumerate(["10_pp", "20_pp", "lit"]):
    ch = (mon.loc["Year", s] - mon.loc["Year", "base"])
    ax.bar(mm.index + (j - 1) * bw, (mm[s] - mm["base"]) * 1e3, width=bw, color=COL[s],
           label=f"{SCEN[s][2]}: {ch:+.2f} TWh/yr ({100 * ch / mon.loc['Year', 'base']:+.2f}%)")
ax.axhline(0, color="black", lw=0.8)
ax.set_title("Change in monthly peninsular demand vs base (M4, 2025 calendar and level)")
ax.set_ylabel("GWh vs base")
ax.set_xticks(range(1, 13), ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"])
ax.legend(fontsize=9, loc="upper left")
ax.grid(alpha=0.3, axis="y")
fig.tight_layout()
fig.savefig(OUT / "2_monthly_demand_change_2025.png", dpi=150, facecolor="white")
plt.close(fig)

# --- Plot 3: daily demand and daily peak ---------------------------------------
daily = D.groupby("date")[cols].agg(["sum", "max"])
fig, ax = plt.subplots(2, 1, figsize=(12, 8.5), sharex=True)
for s, (_, _, lab) in SCEN.items():
    ax[0].plot(daily.index, daily[(s, "sum")].rolling(7, center=True, min_periods=1).mean() / 1e3,
               color=COL[s], lw=2 if s == "base" else 1.4, label=lab)
    ax[1].plot(daily.index, daily[(s, "max")].rolling(7, center=True, min_periods=1).mean() / 1e3,
               color=COL[s], lw=2 if s == "base" else 1.4, label=f"{lab} (annual peak {daily[(s, 'max')].max() / 1e3:.1f} GW)")
ax[0].plot(daily.index, daily[("actual_2025", "sum")].rolling(7, center=True, min_periods=1).mean() / 1e3,
           color="grey", lw=1, ls=":", label="Actual 2025 demand (reference)")
ax[0].set_title("Daily peninsular demand, 7-day average")
ax[0].set_ylabel("GWh per day")
ax[1].set_title("Daily peak hour, 7-day average")
ax[1].set_ylabel("GW")
for a in ax:
    a.grid(alpha=0.3)
    a.legend(fontsize=8.5, loc="upper center", ncol=2)
    for ms, c in [(WINTER, "#deebf7"), (SUMMER, "#fee0d2")]:
        pass
ax[0].set_ylim(top=ax[0].get_ylim()[1] * 1.06)
ax[1].set_ylim(top=ax[1].get_ylim()[1] * 1.06)
ax[1].xaxis.set_major_locator(mdates.MonthLocator())
ax[1].xaxis.set_major_formatter(mdates.DateFormatter("%b"))
fig.tight_layout()
fig.savefig(OUT / "3_daily_demand_and_peak_2025.png", dpi=150, facecolor="white")
plt.close(fig)

pd.set_option("display.width", 250)
print(S.round(2).to_string())
print(mon.round(2).to_string())
