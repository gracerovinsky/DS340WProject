# Predicting Aircraft Delay Recovery at Major U.S. Hubs
### Do Weather and Scheduled Traffic Help Across Airports and Years?

Grace Rovinsky, Nurnayli Qistina Mohd Harith  |  DS340W, Fall 2026

**Question.** When an aircraft arrives late (>= 15 min) at a hub, does its next departure leave < 15 min late
("recovery")? Do weather and scheduled airport traffic improve that prediction, and is the improvement reliable across
**airports, operating conditions and years** (train 2019 -> test 2020, 2021, 2022)?

**Airports (chosen for different weather/traffic regimes):** ATL (baseline hub), ORD (snow/wind), DEN (snow/wind),
SFO (fog/low ceilings), JFK (congestion), SEA (low cloud). Edit `AIRPORTS` in `config.R` to change.

## Pipeline (run from repo root)
| Step | Script | What it does |
|---|---|---|
| 0 | `scripts/00_download_weather.sh` | Download NOAA hourly weather for all airports/years |
| 1 | `scripts/01_build_flights.R` | Load flight CSVs, clean, build UTC timestamps (DST-safe), validate arrival times |
| 2 | `scripts/02_clean_weather.R` | Decode NOAA fields per airport, apply quality flags, FAA flight category; prints each station's name (sanity check) |
| 3 | `scripts/03_build_connections.R` | Aircraft sequences (incl. cancelled flights), scheduled-only filters, labels, traffic, weather join, funnel table per airport |
| 4 | `scripts/04_models.R` | Baseline vs expanded logistic (+XGBoost if installed); metrics by year / airport / condition; day-level bootstrap CIs; leave-one-airport-out; figures |
| 5 | `scripts/05_demo.R` | Live demo (what-if or replay of held-out connections) |

All settings (years, airports, thresholds, split) live in `config.R`.

## Data (not in git) under `$DS340_DATA_DIR` (default `~/ds340_data`)
- `flights/Combined_Flights_2019.csv` ... `2022.csv` (Kaggle: robikscube/flight-delay-dataset-20182022; monthly `Flights_2019_1.csv` also works)
- `weather/<station>_<year>.csv` (step 0 creates these). `airports.dat` auto-downloads from OpenFlights on first run
  (do the first run on a machine with internet, not a Delta compute node).

## Run
```bash
export DS340_DATA_DIR=$HOME/ds340_data
bash scripts/00_download_weather.sh
Rscript scripts/01_build_flights.R
Rscript scripts/02_clean_weather.R
Rscript scripts/03_build_connections.R
Rscript scripts/04_models.R
Rscript scripts/05_demo.R replay=8
Rscript scripts/05_demo.R airport=SFO arr_delay=40 turnaround=70 vis=0.8 temp=12 wind=3
```
On Delta: edit and submit `slurm/run_pipeline.sbatch`.

## Design decisions (for the report)
- **Prediction time** = actual arrival of the incoming flight; all predictors are known then.
- **Filters use scheduled information only** (turnaround 30-360 min), never outcomes.
- **Sequences include cancelled/diverted flights**, so skipped flights cannot create false connections.
- **Cancelled next flights** excluded from the label (counts in `results/eligible_next_flight_cancelled.csv`).
- Models include an **airport effect**; traffic is scaled **relative to each airport's 2019 median** so busy and small hubs are comparable.
- Weather matched on observation time (public release times unavailable - limitation). Traffic = *scheduled* movements, not congestion.
- Reliability is measured by **change in Brier score (expanded - baseline)** with day-level bootstrap CIs.

## Work log / AI use
`DS340W_Hours_Log.xlsx` (hours + AI-use log). Disclose all AI use in the report and the presentation (course requirement).
