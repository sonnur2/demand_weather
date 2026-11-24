"""Actual 2025 vs M4 scenario series: national HDD/CDD and demand.

Answers the supervisor's question on why standard M4 is below actual 2025 in summer.
Weather: national HDD14/CDD21 = small-consumer-weighted mean over 47 provinces
(same weights as M4). Actual 2025 from HDD14_CDD21.xlsx; 2030 scenarios from
Eduardo's files, matched by calendar date (month-day).
Demand (MW): actual 2025 (REE peninsula); M4 fitted with actual 2025 weather
(2025 calendar and level); standard M4 on the 2030 calendar (previous files);
standard and extreme M4 on the 2025 calendar (new files).
Run from the repository root after scripts/project_2025_calendar.py.
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
w = pd.read_csv("output/baseline/province_weights.csv").set_index("province").w


def norm(s):
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().upper().strip()


def national(path, year=None):
    parts = []
    for s, d in pd.read_excel(path, sheet_name=None).items():
        if not s[:2].isdigit():
            continue
        d = d.iloc[:, :6].copy()
        d.columns = ["date", "tmin", "tmax", "tavg", "hdd", "cdd"]
        d["date"] = pd.to_datetime(d.date)
        if year:
            d = d[d.date.dt.year == year]
        for c in ["hdd", "cdd", "tavg"]:
            d[c] = d[c] * w[norm(s[3:])]
        parts.append(d)
    g = pd.concat(parts).groupby("date")[["hdd", "cdd", "tavg"]].sum().reset_index()
    g["md"] = g.date.dt.strftime("%m-%d")
    return g.drop(columns="date")


act = national("raw_data/HDD14_CDD21.xlsx", 2025)
std = national("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx")
ext = national("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_1991.xlsx")
wx = act.add_suffix("_actual2025").rename(columns={"md_actual2025": "md"})
wx = wx.merge(std.add_suffix("_standard").rename(columns={"md_standard": "md"}), on="md")
wx = wx.merge(ext.add_suffix("_extreme").rename(columns={"md_extreme": "md"}), on="md")
wx.insert(0, "date", pd.to_datetime("2025-" + wx.md))
wx = wx.drop(columns="md")

# Hourly demand
eff = pd.read_csv("output/baseline/hourly_effects_M4.csv")
eff = eff[eff["sample"].str.startswith("Pen")]
bH = eff[eff["var"] == "hdd"].set_index("hour").pct / 100
bC = eff[eff["var"] == "cdd"].set_index("hour").pct / 100
std25 = pd.read_csv("output/energeia_2025cal/hourly_2025cal_standard_M4.csv", parse_dates=["date"])
ext25 = pd.read_csv("output/energeia_2025cal/hourly_2025cal_extreme_M4.csv", parse_dates=["date"])
std30 = pd.read_csv("output/projections_2030/demand_hourly_2030.csv", usecols=["scenario", "M4"])
std30 = std30[std30.scenario == "std"].M4.values
actual = pd.read_excel("output/energeia/energeia_demand_2025_actual.xlsx", header=None,
                       skiprows=5, usecols=[2]).iloc[:8760, 0].astype(float).values
# M4 with actual 2025 weather = standard 2025-calendar series with the weather multiplier swapped
h = std25.hour.values
x = std25.merge(wx, on="date", how="left")
log_ratio = bH[h].values * (x.hdd_actual2025 - x.hdd_standard).values + bC[h].values * (x.cdd_actual2025 - x.cdd_standard).values
H = pd.DataFrame({"date": std25.date, "hour": h, "actual_2025": actual,
                  "M4_fitted_actual_weather": std25.mw.values * np.exp(log_ratio),
                  "M4_standard_2030calendar_previous": std30,
                  "M4_standard_2025calendar": std25.mw.values,
                  "M4_extreme_2025calendar": ext25.mw.values})
dcols = list(H.columns[2:])

daily = H.groupby("date")[dcols].sum().div(1e3).add_suffix("_GWh").reset_index().merge(wx, on="date")
daily.insert(1, "weekday_2025", daily.date.dt.day_name())
daily["month"] = daily.date.dt.month
agg = {c: "sum" for c in daily.columns if c.endswith("_GWh") or c.startswith(("hdd", "cdd"))}
agg.update({c: "mean" for c in daily.columns if c.startswith("tavg")})
monthly = daily.groupby("month").agg(agg)
for c in [c for c in monthly if c.endswith("_GWh")]:
    monthly[c.replace("_GWh", "_TWh")] = monthly.pop(c) / 1e3
monthly.loc["Year"] = [monthly[c].sum() if not c.startswith("tavg") else monthly[c].mean() for c in monthly]
monthly = monthly.round(2)
daily = daily.drop(columns="month").round(2)

readme = pd.DataFrame({"note": [
    "Actual 2025 vs M4 scenario series (peninsula). Weather is national, small-consumer-weighted (same as M4).",
    "Actual 2025 weather: HDD14_CDD21.xlsx. Scenario weather: Eduardo's 2030 files (standard = from 2024, extreme = from 1991), matched by calendar date.",
    "M4_fitted_actual_weather: M4 with the actual 2025 weather, 2025 calendar and 2025 level (in-sample fit).",
    "M4_standard_2030calendar_previous: the series in the supervisor's comparison (2030 weekdays and holidays laid on 2025 dates).",
    "M4_standard_2025calendar / M4_extreme_2025calendar: the new files (2025 weekdays and holidays).",
    "Demand: hourly MW (= MWh per hour); daily GWh; monthly TWh. HDD/CDD in degree days; tavg in °C.",
]})
with pd.ExcelWriter(OUT / "actual2025_vs_scenarios.xlsx") as xw:
    readme.to_excel(xw, sheet_name="README", index=False)
    monthly.reset_index().to_excel(xw, sheet_name="Monthly", index=False)
    daily.to_excel(xw, sheet_name="Daily", index=False)
    H.round(1).to_excel(xw, sheet_name="Hourly", index=False)

# Chart: summer CDD and daily demand
s = daily[(daily.date >= "2025-05-15") & (daily.date <= "2025-09-30")]
fig, ax = plt.subplots(2, 1, figsize=(11, 7), sharex=True)
ax[0].plot(s.date, s.cdd_actual2025, color="black", lw=1.8, label="Actual 2025")
ax[0].plot(s.date, s.cdd_standard, color="#1f77b4", lw=1.5, label="Standard scenario (2030 from 2024)")
ax[0].plot(s.date, s.cdd_extreme, color="#d62728", lw=1.2, ls="--", label="Extreme scenario (2030 from 1991)")
ax[0].set_ylabel("National CDD (base 21 °C)")
ax[0].set_title("Cooling degree days by calendar date")
ax[0].legend(loc="upper left", fontsize=9)
ax[1].plot(s.date, s.actual_2025_GWh, color="black", lw=1.8, label="Actual 2025 demand")
ax[1].plot(s.date, s["M4_standard_2030calendar_previous_GWh"], color="#9ecae1", lw=1.2,
           label="M4 standard, 2030 calendar (previous file)")
ax[1].plot(s.date, s.M4_standard_2025calendar_GWh, color="#1f77b4", lw=1.5, label="M4 standard, 2025 calendar (new file)")
ax[1].set_ylabel("Daily demand (GWh)")
ax[1].set_title("Peninsular daily demand")
ax[1].legend(loc="upper left", fontsize=9)
for a in ax:
    a.grid(alpha=0.3)
fig.tight_layout()
fig.savefig(OUT / "summer_2025_actual_vs_standard.png", dpi=150, facecolor="white")
print(monthly.to_string())
