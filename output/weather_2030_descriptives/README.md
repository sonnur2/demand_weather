# 2030 weather scenarios: descriptives (no regressions)

Script: `scripts/describe_2030_weather.R`. National series are weighted means over the 47 provinces, with the same small-consumer weights as M1–M4. "Observed" means our 2018–2025 weather file.

## Why the extreme year peaks in winter

The extreme (1991-based) year is **not hotter at its single hottest day** than the standard year, but its **winter is much colder**.

| | Standard (2024-based) | Extreme (1991-based) | Observed 2018–2025 |
| --- | --- | --- | --- |
| Annual HDD | 510 | **754** (+48%) | 551–809 |
| Annual CDD | 436 | **511** (+17%) | 334–491 |
| Hottest day, national CDD | 8.1 (30 Jul, 29.1 °C) | 7.8 (17 Jul, 28.7 °C) | 6.8–8.8 |
| Coldest day, national HDD | 7.7 (8 Jan, 6.3 °C) | **9.1** (14 Jan and 14 Feb, 4.9 °C) | 8.0–10.9 |
| Days with HDD > 8 | 0 | 7 | 0–17 |
| Days with CDD > 5 | 36 | 41 | 8–41 |

**Summer.** The extreme summer is hot because hot weather lasts longer, not because the peak is higher. June CDD is 74 vs 49, and September CDD is 89 vs 37, 2.4 times as much. The hottest days are about the same in both years.

**Winter.** January–February 1991 was cold, and the 2030 correction does not remove that:

- February HDD is 168 vs 79 in the standard year (2.1×). The 2018–2025 February mean is 120 and the maximum is 196.
- A cold spell on 11–15 February has a national mean of about 5 °C and minimum temperatures around 0 °C. All 8 coldest days of either 2030 year are in the extreme year (14–16 January and 11–15 February).
- The extreme year's January (185) and November (84) HDD are also well above the standard year's (121 and 33).
- The cold is broad. Mean daily February HDD is about 1.7–3.5 times higher in every zone: Interior 8.3 vs 4.9, North 6.6 vs 3.5, Mediterranean 5.0 vs 1.7, South 4.3 vs 1.4.

**Peak hour.** Where the peak lands follows from these numbers (M4 decomposition, MW):

| Hour | Demand without weather | + weather | = projected |
| --- | --- | --- | --- |
| Extreme, 14 Feb 20h | 32,889 | +6,526 | **39,415** (annual peak) |
| Extreme, hottest hour (17 Jul 17h) | 27,865 | +10,393 | 38,258 |
| Standard, 30 Jul 17h | 27,834 | +10,924 | **38,758** (annual peak) |
| Standard, 14 Feb 20h | 32,889 | +500 | 33,389 |

- **Weather effect:** the extra demand from weather is about 10–11 GW on the hottest summer afternoon in both years.
- **Baseline:** a February evening starts about 5 GW higher than a July afternoon (lighting plus the evening load), even with no weather.
- **Result:** in the standard year, mild winter days add little, so the summer afternoon wins. In the extreme year, the February cold spell adds 6.5 GW on top of the high winter baseline, which beats the summer peak by about 1 GW.

(The extreme-scenario peak is 14 February at 20:00, not 20 February.)

## Files

- `annual_totals.csv`: HDD, CDD, mean temperature and extreme-day counts for each scenario and each observed year.
- `monthly_totals.csv`: monthly HDD, CDD and temperature for both 2030 years, against the 2018–2025 mean and maximum.
- `top10_days.csv`: the 10 coldest and 10 hottest days of each 2030 year.
- `cold_spells.csv`: runs of days with HDD above the 2018–2025 99th percentile (8.6).
- `peak_days_weather.csv`: weather on the projected peak days.
- `february_mean_daily_hdd_by_zone.csv`: February HDD by climate zone.
- `daily_temperature_2030.png`: daily national temperature, both years, against the 2018–2025 range.
- `monthly_degree_days_2030.png`: monthly HDD and CDD bars.
