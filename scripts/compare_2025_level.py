"""Actual demand and weather (2021-2025) next to the 2030 projections.

Answers: which demand level do the 2030 projections start from?
The level is 2025 (2025 year fixed effect); the calendar shape is estimated on
June 2021 - Dec 2025. This script puts actual 2025 demand and degree-days
beside the projected 2030 years so the two can be compared.

Output: output/level_2025_vs_2030/level_2025_vs_2030.xlsx
Run from the repository root:  python3 scripts/compare_2025_level.py
"""
import unicodedata
import pandas as pd
import openpyxl

def norm(x):
    x = unicodedata.normalize("NFKD", x.strip().upper())
    return "".join(c for c in x if not unicodedata.combining(c))

# Actual hourly peninsular demand (local clock hour, as in prepare_data.R)
d = pd.read_csv("raw_data/hourly_demand_community_2015_2025.csv", usecols=["datetime", "peninsula"], dtype={"datetime": str})
d["local"] = pd.to_datetime(d["datetime"].str[:19])
d["date"] = d["local"].dt.normalize()
d["hour"] = d["local"].dt.hour
act = d.groupby(["date", "hour"], as_index=False)["peninsula"].mean().rename(columns={"peninsula": "demand_mw"})
act = act[(act.date >= "2021-01-01") & (act.date <= "2025-12-31")]

# Actual national HDD/CDD, small-consumer weights (same as M1-M4)
w = pd.read_csv("output/baseline/province_weights.csv").set_index("province")["w"]
wb = openpyxl.load_workbook("raw_data/HDD14_CDD21.xlsx", read_only=True)
rows = []
for s in wb.sheetnames:
    if not s[:2].isdigit():
        continue
    p = norm(s[3:])
    for r in wb[s].iter_rows(min_row=2, values_only=True):
        if r[0] is not None:
            rows.append((pd.Timestamp(r[0]).normalize(), p, r[3], r[4], r[5]))
wx = pd.DataFrame(rows, columns=["date", "province", "tavg", "hdd", "cdd"])
assert set(wx.province) == set(w.index), set(wx.province) ^ set(w.index)
wx["w"] = wx.province.map(w)
nat = wx.assign(**{c: wx[c] * wx.w for c in ["tavg", "hdd", "cdd"]}).groupby("date")[["tavg", "hdd", "cdd"]].sum().reset_index()

# 2030 projections (M4) and 2030 weather
proj = pd.read_csv("output/projections_2030/demand_hourly_2030.csv")
proj = proj[proj.scenario.isin(["std", "extreme", "no_weather"])]
proj["date"] = pd.to_datetime(proj["date"])
w30 = {}
for lab, f in [("std", "2024"), ("extreme", "1991")]:
    wb = openpyxl.load_workbook(f"raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_{f}.xlsx", read_only=True)
    rr = []
    for s in wb.sheetnames:
        if not s[:2].isdigit():
            continue
        p = norm(s[3:])
        for r in wb[s].iter_rows(min_row=2, values_only=True):
            if r[0] is not None:
                rr.append((pd.Timestamp(r[0]).normalize(), w[p] * r[3], w[p] * r[4], w[p] * r[5]))
    w30[lab] = pd.DataFrame(rr, columns=["date", "tavg", "hdd", "cdd"]).groupby("date").sum().reset_index()
w30["no_weather"] = w30["std"].assign(hdd=0.0, cdd=0.0, tavg=float("nan"))

# ---- Annual table
ann = []
a = act.merge(nat, on="date")
for y, g in a.groupby(a.date.dt.year):
    daily = g.drop_duplicates("date")
    note = "blackout 28-29 Apr included" if y == 2025 else ""
    ann.append(dict(series=f"Actual {y}", twh=g.demand_mw.sum() / 1e6, mean_mw=g.demand_mw.mean(),
                    peak_mw=g.demand_mw.max(), peak_hour=str(g.loc[g.demand_mw.idxmax(), "date"].date()) + f" {int(g.loc[g.demand_mw.idxmax(), 'hour']):02d}h",
                    hdd=daily.hdd.sum(), cdd=daily.cdd.sum(), tavg=daily.tavg.mean(), hours=len(g), note=note))
names = {"std": "2030 standard (2024 weather), M4", "extreme": "2030 extreme (1991 weather), M4", "no_weather": "2030 no weather, M4"}
for s in ["std", "extreme", "no_weather"]:
    g = proj[proj.scenario == s]
    ww = w30[s]
    i = g.M4.idxmax()
    ann.append(dict(series=names[s], twh=g.M4.sum() / 1e6, mean_mw=g.M4.mean(), peak_mw=g.M4.max(),
                    peak_hour=g.loc[i, "datetime"].replace(":00", "h").replace(" ", " "),
                    hdd=ww.hdd.sum(), cdd=ww.cdd.sum(), tavg=ww.tavg.mean(), hours=len(g),
                    note="level = 2025 year effect"))
ann = pd.DataFrame(ann)
avg = ann[ann.series.str.startswith("Actual")][["twh", "mean_mw", "peak_mw", "hdd", "cdd", "tavg"]].mean()
ann = pd.concat([ann, pd.DataFrame([dict(series="Actual 2021-2025 average", **avg, note="not used as the level")])], ignore_index=True)

# ---- Monthly table: actual 2025 vs 2030 M4
a25 = a[a.date.dt.year == 2025]
mon = pd.DataFrame({"month": range(1, 13)})
mon["actual_2025_twh"] = a25.groupby(a25.date.dt.month).demand_mw.sum().values / 1e6
d25 = nat[nat.date.dt.year == 2025]
mon["actual_2025_hdd"] = d25.groupby(d25.date.dt.month).hdd.sum().values
mon["actual_2025_cdd"] = d25.groupby(d25.date.dt.month).cdd.sum().values
for s in ["std", "extreme", "no_weather"]:
    g = proj[proj.scenario == s]
    mon[f"{s}_2030_twh"] = g.groupby(g.date.dt.month).M4.sum().values / 1e6
    ww = w30[s]
    mon[f"{s}_2030_hdd"] = ww.groupby(ww.date.dt.month).hdd.sum().values
    mon[f"{s}_2030_cdd"] = ww.groupby(ww.date.dt.month).cdd.sum().values

# ---- Hourly side by side: actual 2025 and 2030 (same calendar date and hour)
h25 = a25.rename(columns={"demand_mw": "actual_2025_mw", "hdd": "actual_2025_hdd", "cdd": "actual_2025_cdd"})[
    ["date", "hour", "actual_2025_mw", "actual_2025_hdd", "actual_2025_cdd"]]
h25["md"] = h25.date.dt.strftime("%m-%d")
hp = proj.pivot_table(index=["date", "hour"], columns="scenario", values="M4").reset_index()
hp["md"] = hp.date.dt.strftime("%m-%d")
for s in ["std", "extreme"]:
    hp = hp.merge(w30[s][["date", "hdd", "cdd"]].rename(columns={"hdd": f"{s}_2030_hdd", "cdd": f"{s}_2030_cdd"}), on="date")
hour = hp.merge(h25.drop(columns="date"), on=["md", "hour"], how="left")
hour = hour.rename(columns={"date": "date_2030", "std": "std_2030_M4_mw", "extreme": "extreme_2030_M4_mw", "no_weather": "noweather_2030_M4_mw"})
hour = hour[["date_2030", "hour", "actual_2025_mw", "std_2030_M4_mw", "extreme_2030_M4_mw", "noweather_2030_M4_mw",
             "actual_2025_hdd", "actual_2025_cdd", "std_2030_hdd", "std_2030_cdd", "extreme_2030_hdd", "extreme_2030_cdd"]]

readme = pd.DataFrame({"note": [
    "Which demand level do the 2030 projections start from?",
    "The 2025 level. Each projection = calendar effects (hour x month, hour x day type, estimated on Jun 2021 - Dec 2025) + the 2025 year fixed effect + weather effect of the 2030 degree-days.",
    "It is not the 2021-2025 average: each year has its own fixed effect, and 2030 is given the 2025 one.",
    "So the files describe roughly 2025-sized demand under 2030 weather. They match a 2025-like supply side unless rescaled to a 2030 demand forecast.",
    "Dependent variable (LHS) of the peninsula regressions: log of hourly peninsular demand in MW (hourly mean = MWh in that hour).",
    "Degree-days: national = province values weighted by mean small-consumer demand (same weights as M1-M4).",
    "Sheet Annual: actual 2021-2025 vs the 2030 projections (M4). Actual 2025 includes the 28-29 April blackout (dropped from the regressions).",
    "Sheet Monthly_2025_vs_2030: actual 2025 by month vs 2030 standard, extreme and no-weather (M4).",
    "Sheet Hourly_2025_vs_2030: actual 2025 next to 2030 M4 on the same calendar date and hour (2025 and 2030 weekdays differ). Actual 2025 follows clock time, so the March DST hour is missing.",
]})
out = "output/level_2025_vs_2030/level_2025_vs_2030.xlsx"
with pd.ExcelWriter(out, engine="openpyxl") as x:
    readme.to_excel(x, sheet_name="README", index=False)
    ann.round(3).to_excel(x, sheet_name="Annual", index=False)
    mon.round(3).to_excel(x, sheet_name="Monthly_2025_vs_2030", index=False)
    hour.round({c: 2 for c in hour.columns if c != "date_2030"}).to_excel(x, sheet_name="Hourly_2025_vs_2030", index=False)
pd.set_option("display.width", 250)
print(ann.round(2).to_string()); print(mon.round(2).to_string())
