"""Write 2030 peninsular demand projections into the Energeia demand template.

One file per model x scenario in output/energeia/:
    energeia_demand_2030_standard_<model>.xlsx      2030 weather from 2024 (Eduardo)
    energeia_demand_2030_extreme_<model>.xlsx       2030 weather from 1991 (Eduardo)
    energeia_demand_2030_noweather_<model>.xlsx     benchmark, HDD = CDD = 0 every day
    energeia_demand_2030_event<k>_<name>_<model>.xlsx
        standard year with heat wave k inserted on its dates (one event per file,
        because the 8 events overlap in time)

Input:  output/projections_2030/demand_hourly_2030.csv         (from project_2030.R)
        output/projections_2030/demand_hourly_2030_events.csv  (event dates only)
        raw_data/energeia_demand_template.xlsx                 (sheet Template_demand)
Run from the repository root:  python3 scripts/export_energeia.py
"""
import csv
import datetime as dt
import re
from collections import defaultdict
from pathlib import Path

import openpyxl

TEMPLATE = Path("raw_data/energeia_demand_template.xlsx")
SOURCE = Path("output/projections_2030/demand_hourly_2030.csv")
EVENTS = Path("output/projections_2030/demand_hourly_2030_events.csv")
OUT = Path("output/energeia")
YEAR = 2030
MODELS = ["M1", "M2", "M3", "M4", "N2", "N4", "Z2", "Z4"]
FIRST_ROW = 6          # first data row of Template_demand
N_HOURS = 8760
BASE = {"std": "standard", "extreme": "extreme", "no_weather": "noweather"}

# Full-year series: label -> {datetime: row}
years = defaultdict(dict)
with SOURCE.open(newline="") as f:
    for r in csv.DictReader(f):
        if r["scenario"] in BASE:
            years[BASE[r["scenario"]]][r["datetime"]] = r

# Event years: the standard year with the event's hours replaced
ev_rows, ev_names = defaultdict(dict), {}
with EVENTS.open(newline="") as f:
    for r in csv.DictReader(f):
        ev_rows[int(r["id"])][r["datetime"]] = {m: f"{float(r[m]):.1f}" for m in MODELS}
        ev_names[int(r["id"])] = r["event"]
for k in sorted(ev_rows):
    slug = re.sub(r"[^A-Za-z0-9]", "", ev_names[k].replace("September", "Sep"))
    label = f"event{k}_{slug}"
    years[label] = {t: {**row, **ev_rows[k].get(t, {})} for t, row in years["standard"].items()}

for label, rows in years.items():
    assert len(rows) == N_HOURS, (label, len(rows))

OUT.mkdir(parents=True, exist_ok=True)
for old in OUT.glob("energeia_demand_2030_*.xlsx"):
    old.unlink()
for label, rows in years.items():
    times = sorted(rows)
    assert times[0] == f"{YEAR}-01-01 00:00" and times[-1] == f"{YEAR}-12-31 23:00"
    for m in MODELS:
        wb = openpyxl.load_workbook(TEMPLATE)
        ws = wb["Template_demand"]
        # Raw 2026 data sheet of the example file is not part of the template
        for name in [n for n in wb.sheetnames if n != "Template_demand"]:
            del wb[name]
        ws["E1"] = YEAR
        ws.cell(FIRST_ROW, 2).value = dt.datetime(YEAR, 1, 1, 0, 0)  # later rows: formula +1h
        ws["D5"] = None
        for i, t in enumerate(times):
            ws.cell(FIRST_ROW + i, 3).value = float(rows[t][m])
            ws.cell(FIRST_ROW + i, 4).value = None  # 2026 reference timestamps of the example file
        path = OUT / f"energeia_demand_2030_{label}_{m}.xlsx"
        wb.save(path)
        print(path)
