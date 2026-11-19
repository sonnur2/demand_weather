"""Write 2030 peninsular demand projections into the Energeia demand template.

One file per model x scenario:
    output/energeia/energeia_demand_2030_<scenario>_<model>.xlsx

Input:  output/projections_2030/demand_hourly_2030.csv (from project_2030.R)
        raw_data/energeia_demand_template.xlsx (sheet Template_demand)
Run from the repository root:  python3 scripts/export_energeia.py
"""
import csv
import datetime as dt
from collections import defaultdict
from pathlib import Path

import openpyxl

TEMPLATE = Path("raw_data/energeia_demand_template.xlsx")
SOURCE = Path("output/projections_2030/demand_hourly_2030.csv")
OUT = Path("output/energeia")
YEAR = 2030
MODELS = ["M1", "M2", "M3", "M4", "N2", "N4", "Z2", "Z4"]
SCENARIOS = {"std": "standard", "extreme": "extreme", "no_weather": "noweather"}
FIRST_ROW = 6          # first data row of Template_demand
N_HOURS = 8760

# Read hourly series: scenario -> list of rows in time order
series = defaultdict(list)
with SOURCE.open(newline="") as f:
    for r in csv.DictReader(f):
        if r["scenario"] in SCENARIOS:
            series[r["scenario"]].append(r)
for s, rows in series.items():
    rows.sort(key=lambda r: r["datetime"])
    assert len(rows) == N_HOURS, (s, len(rows))
    assert rows[0]["datetime"] == f"{YEAR}-01-01 00:00" and rows[-1]["datetime"] == f"{YEAR}-12-31 23:00"

OUT.mkdir(parents=True, exist_ok=True)
for s, label in SCENARIOS.items():
    for m in MODELS:
        wb = openpyxl.load_workbook(TEMPLATE)
        ws = wb["Template_demand"]
        # Raw 2026 data sheet of the example file is not part of the template
        for name in [n for n in wb.sheetnames if n != "Template_demand"]:
            del wb[name]
        ws["E1"] = YEAR
        ws.cell(FIRST_ROW, 2).value = dt.datetime(YEAR, 1, 1, 0, 0)  # later rows: formula +1h
        for i, r in enumerate(series[s]):
            row = FIRST_ROW + i
            ws.cell(row, 3).value = float(r[m])
            ws.cell(row, 4).value = None  # 2026 reference timestamps of the example file
        ws["D5"] = None
        path = OUT / f"energeia_demand_2030_{label}_{m}.xlsx"
        wb.save(path)
        print(path)
