"""Actual 2025 hourly peninsular demand in the Energeia template (for comparison).

Source: raw_data/hourly_demand_community_2015_2025.csv, column `peninsula`
(MW hourly mean = MWh in the hour), local clock time as in prepare_data.R.
The template has 8,760 clock hours with no DST shift: the hour skipped on
30 March 2025 (02:00) is filled with the mean of 01:00 and 03:00, and the
repeated hour on 26 October is averaged.
Output: output/energeia/energeia_demand_2025_actual.xlsx
"""
import datetime as dt
import pandas as pd
import openpyxl

d = pd.read_csv("raw_data/hourly_demand_community_2015_2025.csv", usecols=["datetime", "peninsula"], dtype={"datetime": str})
d["local"] = pd.to_datetime(d["datetime"].str[:19])
d = d[d.local.dt.year == 2025].groupby("local")["peninsula"].mean()
full = pd.date_range("2025-01-01 00:00", "2025-12-31 23:00", freq="h")
missing = full.difference(d.index)
s = d.reindex(full).interpolate()
print("filled hours:", list(missing.astype(str)), "| TWh:", round(s.sum() / 1e6, 2))
assert len(s) == 8760

wb = openpyxl.load_workbook("raw_data/energeia_demand_template.xlsx")
for name in [n for n in wb.sheetnames if n != "Template_demand"]:
    del wb[name]
ws = wb["Template_demand"]
ws["E1"] = 2025
ws["D5"] = None
ws.cell(6, 2).value = dt.datetime(2025, 1, 1)
for i, v in enumerate(s.values):
    ws.cell(6 + i, 3).value = float(round(v, 1))
    ws.cell(6 + i, 4).value = None
wb.save("output/energeia/energeia_demand_2025_actual.xlsx")
