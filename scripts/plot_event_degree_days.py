"""Daily national degree days in each heat-wave window: event vs standard vs actual 2025.

National = small-consumer-weighted mean of the 47 provincial values (same as M4).
Event and standard: Eduardo's 2030 files (heat_events_2030.xlsx, ..._from_2024.xlsx).
Actual 2025: observed provincial file HDD14_CDD21.xlsx. Matched by calendar date.
Output: output/compare_2025_actual/event_windows/ ; run from the repository root.
"""
import unicodedata
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import pandas as pd

OUT = Path("output/compare_2025_actual/event_windows")
OUT.mkdir(parents=True, exist_ok=True)
w = pd.read_csv("output/baseline/province_weights.csv").set_index("province").w


def norm(s):
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().upper().strip()


def national_file(path, year=None):
    parts = []
    for s, d in pd.read_excel(path, sheet_name=None).items():
        if not s[:2].isdigit():
            continue
        d = d.iloc[:, :6].copy()
        d.columns = ["date", "tmin", "tmax", "tavg", "hdd", "cdd"]
        d["date"] = pd.to_datetime(d.date)
        if year:
            d = d[d.date.dt.year == year]
        d["hdd"] *= w[norm(s[3:])]
        d["cdd"] *= w[norm(s[3:])]
        parts.append(d[["date", "hdd", "cdd"]])
    g = pd.concat(parts).groupby("date").sum()
    g.index = pd.to_datetime("2025-" + g.index.strftime("%m-%d"))
    return g


act = national_file("raw_data/HDD14_CDD21.xlsx", 2025)
std = national_file("raw_data/province_daily_degree_days_HDD14_CDD21_2030_from_2024.xlsx")
book = pd.read_excel("raw_data/heat_events_2030.xlsx", sheet_name=None)
events = {}
for name, e in book.items():
    if name == "Summary":
        continue
    idx = pd.to_datetime("2025-" + pd.to_datetime(e.iloc[:, 0]).dt.strftime("%m-%d"))
    out = {}
    for v, tag in [("cdd", "CDD21"), ("hdd", "HDD14")]:
        cols = [c for c in e.columns if f"{tag} (" in c and not c.startswith("Mainland")]
        out[v] = sum(e[c].values * w[norm(c.split(f" {tag}")[0])] for c in cols)
    events[name] = pd.DataFrame(out, index=idx)

C = {"event": "#d62728", "std": "#1f77b4", "act": "black"}
rows, frames = [], []


def panel(ax, name, ev, show_legend=True):
    pad = pd.Timedelta(days=7)
    lo, hi = ev.index.min(), ev.index.max()
    a, s = act.loc[lo - pad:hi + pad], std.loc[lo - pad:hi + pad]
    ax.axvspan(lo - pd.Timedelta(hours=12), hi + pd.Timedelta(hours=12), color="#fde0dd", alpha=0.6, lw=0,
               label="Event window")
    ax.plot(a.index, a.cdd, color=C["act"], lw=1.8, marker="o", ms=2.5,
            label=f"Actual 2025 ({act.loc[lo:hi].cdd.sum():.0f} in window)")
    ax.plot(s.index, s.cdd, color=C["std"], lw=1.8, marker="o", ms=2.5,
            label=f"Standard scenario ({std.loc[lo:hi].cdd.sum():.0f})")
    ax.plot(ev.index, ev.cdd, color=C["event"], lw=2.2, marker="o", ms=3,
            label=f"Event ({ev.cdd.sum():.0f})")
    hd = {"Actual 2025": act.loc[lo:hi].hdd.sum(), "Standard": std.loc[lo:hi].hdd.sum(), "Event": ev.hdd.sum()}
    title = f"Event {name}: {lo:%d %b} to {hi:%d %b}"
    if max(hd.values()) >= 1:
        title += "\nHDD in window: " + ", ".join(f"{k} {v:.0f}" for k, v in hd.items())
    ax.set_title(title, fontsize=10.5)
    ax.set_ylabel("National CDD (base 21 °C)")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%d %b"))
    ax.grid(alpha=0.3)
    ax.set_ylim(bottom=0)
    if show_legend:
        ax.legend(loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=4, fontsize=8.5, frameon=False,
                  title="In brackets: total CDD over the event dates", title_fontsize=8.5)


for name, ev in events.items():
    fig, ax = plt.subplots(figsize=(9, 5))
    panel(ax, name, ev)
    fig.autofmt_xdate()
    fig.tight_layout()
    fig.savefig(OUT / f"event_{name.split()[0]}_{''.join(name.split()[1:]).replace('-', '')}.png", dpi=150, facecolor="white")
    plt.close(fig)
    lo, hi = ev.index.min(), ev.index.max()
    rows.append({"event": name, "start": lo.date(), "end": hi.date(), "days": len(ev),
                 "CDD_actual_2025": act.loc[lo:hi].cdd.sum(), "CDD_standard": std.loc[lo:hi].cdd.sum(), "CDD_event": ev.cdd.sum(),
                 "HDD_actual_2025": act.loc[lo:hi].hdd.sum(), "HDD_standard": std.loc[lo:hi].hdd.sum(), "HDD_event": ev.hdd.sum()})
    frames.append(pd.DataFrame({"event": name, "date": ev.index.date, "CDD_actual_2025": act.loc[lo:hi].cdd.values,
                                "CDD_standard": std.loc[lo:hi].cdd.values, "CDD_event": ev.cdd.values,
                                "HDD_actual_2025": act.loc[lo:hi].hdd.values, "HDD_standard": std.loc[lo:hi].hdd.values,
                                "HDD_event": ev.hdd.values}))

fig, axes = plt.subplots(4, 2, figsize=(15, 20))
for ax, (name, ev) in zip(axes.flat, events.items()):
    panel(ax, name, ev)
    for lab in ax.get_xticklabels():
        lab.set_rotation(30)
fig.suptitle("National CDD in each heat-wave window: event vs standard scenario vs actual 2025\n"
             "(small-consumer-weighted mean of provincial degree days; shaded = event dates; 7 days shown either side)",
             fontsize=12.5)
fig.tight_layout(rect=(0, 0, 1, 0.965))
fig.savefig(OUT / "all_events_cdd.png", dpi=150, facecolor="white")

with pd.ExcelWriter(OUT / "event_windows_degree_days.xlsx") as xw:
    pd.DataFrame(rows).round(1).to_excel(xw, sheet_name="Totals", index=False)
    pd.concat(frames).round(2).to_excel(xw, sheet_name="Daily", index=False)
print(pd.DataFrame(rows).round(0).to_string())
