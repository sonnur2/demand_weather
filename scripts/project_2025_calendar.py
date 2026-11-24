"""M4 demand for "2025 with 2030 weather" in the Energeia template.

Same M4 model, same 2025 level (2025 year effect), but laid on the 2025
calendar (2025 dates, weekdays, national holidays incl. Easter 2025). The
weather of each calendar day (month-day) comes from the 2030 scenario files.

Method (exact, no re-estimation): log M4 prediction = a(hour, month)
+ g(hour, day type) + weather term. From the 2030 no-weather projection we
recover a + g by least squares (exact: the model is additive in these). The
weather term of a scenario for a given month-day and hour is
log(scenario) - log(no weather) on that 2030 date, which equals
beta_h^H * HDD + beta_h^C * CDD and does not depend on the calendar.
Then 2025 prediction = exp(a + g(2025 day type) + weather term).

Output: output/energeia_2025cal/energeia_demand_2025cal_<scenario>_M4.xlsx
Run from the repository root:  python3 scripts/project_2025_calendar.py
"""
import datetime as dt
import re
from pathlib import Path

import numpy as np
import openpyxl
import pandas as pd

OUT = Path("output/energeia_2025cal")
OUT.mkdir(parents=True, exist_ok=True)


def easter(y):
    a, b, c = y % 19, y // 100, y % 100
    d, e, f = b // 4, b % 4, (b + 8) // 25
    g = (b - f + 1) // 3
    h = (19 * a + b - d - g + 15) % 30
    i, k = c // 4, c % 4
    l = (32 + 2 * e + 2 * i - h - k) % 7
    m = (a + 11 * h + 22 * l) // 451
    return dt.date(y, (h + l - 7 * m + 114) // 31, (h + l - 7 * m + 114) % 31 + 1)


def calendar(y):
    """Same day types as prepare_data.R: day of week (1 = Sunday), holidays own type."""
    fixed = ["01-01", "01-06", "05-01", "08-15", "10-12", "11-01", "12-06", "12-08", "12-25"]
    es = easter(y)
    hol = {dt.date.fromisoformat(f"{y}-{f}") for f in fixed} | {es - dt.timedelta(2), es - dt.timedelta(3)}
    days = pd.date_range(f"{y}-01-01", f"{y}-12-31", freq="D")
    c = pd.DataFrame({"date": days})
    c["md"] = c.date.dt.strftime("%m-%d")
    c["month"] = c.date.dt.month
    dow = (c.date.dt.dayofweek + 1) % 7 + 1          # R wday: 1 = Sunday
    c["daytype"] = np.where(c.date.dt.date.isin(hol), "holiday", dow.astype(str))
    return c


# 2030 projections (M4), long by date-hour
p = pd.read_csv("output/projections_2030/demand_hourly_2030.csv", usecols=["scenario", "date", "hour", "M4"])
p["md"] = p.date.str[5:]
wide = p.pivot_table(index=["md", "hour"], columns="scenario", values="M4")
ev = pd.read_csv("output/projections_2030/demand_hourly_2030_events.csv", usecols=["id", "event", "date", "hour", "M4"])
ev["md"] = ev.date.str[5:]

# Weather terms (log scenario - log no weather) by month-day and hour
wterm = {s: np.log(wide[s]) - np.log(wide["no_weather"]) for s in ["std", "extreme"]}
names = {"std": "standard", "extreme": "extreme"}
for k, g in ev.groupby("id"):
    e = wterm["std"].copy()
    idx = pd.MultiIndex.from_arrays([g.md, g.hour])
    e.loc[idx] = np.log(g.M4.values) - np.log(wide["no_weather"].loc[idx].values)
    lab = f"event{k}_" + re.sub(r"[^A-Za-z0-9]", "", g.event.iloc[0].replace("September", "Sep"))
    wterm[lab] = e
    names[lab] = lab

# Calendar part a(h, m) + g(h, daytype) from the 2030 no-weather projection
c30 = calendar(2030)
nw = wide["no_weather"].rename("nw").reset_index().merge(c30[["md", "month", "daytype"]], on="md")
c25 = calendar(2025)
types = sorted(set(nw.daytype) | set(c25.daytype))
months = list(range(1, 13))


def design(df):
    X = np.zeros((len(df), 12 + len(types) - 1))
    X[np.arange(len(df)), df.month.values - 1] = 1
    for j, t in enumerate(types[1:]):
        X[:, 12 + j] = (df.daytype.values == t)
    return X


cal = {}
max_res = 0
for h in range(24):
    d = nw[nw.hour == h]
    coef, *_ = np.linalg.lstsq(design(d), np.log(d.nw.values), rcond=None)
    max_res = max(max_res, np.abs(design(d) @ coef - np.log(d.nw.values)).max())
    cal[h] = coef
print(f"calendar fit on 2030 no-weather: max |log residual| = {max_res:.2e}")


def build(calendar_df, scen):
    rows = []
    for h in range(24):
        X = design(calendar_df)
        base = X @ cal[h]
        w = wterm[scen].xs(h, level="hour").reindex(calendar_df.md).values
        rows.append(pd.DataFrame({"date": calendar_df.date.values, "hour": h, "mw": np.exp(base + w)}))
    return pd.concat(rows).sort_values(["date", "hour"]).reset_index(drop=True)


# Check: rebuilding the 2030 calendar reproduces the delivered 2030 files
for s in ["std", "extreme"]:
    r = build(c30, s)
    ref = p[p.scenario == s].sort_values(["date", "hour"]).M4.values
    print(f"check 2030 {s}: max |diff| = {np.abs(r.mw.values - ref).max():.2f} MW")

# 2025 calendar
tpl = "raw_data/energeia_demand_template.xlsx"
summary = []
for s, lab in names.items():
    r = build(c25, s)
    assert len(r) == 8760
    wb = openpyxl.load_workbook(tpl)
    for n in [n for n in wb.sheetnames if n != "Template_demand"]:
        del wb[n]
    ws = wb["Template_demand"]
    ws["E1"] = 2025
    ws["D5"] = None
    ws.cell(6, 2).value = dt.datetime(2025, 1, 1)
    for i, v in enumerate(r.mw.values):
        ws.cell(6 + i, 3).value = float(round(v, 1))
        ws.cell(6 + i, 4).value = None
    f = OUT / f"energeia_demand_2025cal_{lab}_M4.xlsx"
    wb.save(f)
    i = r.mw.idxmax()
    summary.append(dict(file=f.name, twh=r.mw.sum() / 1e6, peak_mw=r.mw.max(),
                        peak=f"{r.date[i].date()} {r.hour[i]:02d}h"))
    r.to_csv(OUT / f"hourly_2025cal_{lab}_M4.csv", index=False, float_format="%.1f")
summary = pd.DataFrame(summary)
summary.to_csv(OUT / "summary.csv", index=False)
print(summary.round(2).to_string())
