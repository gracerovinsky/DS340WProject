# ------------------------------------------------------------------
# config.R  --  the ONLY file you should need to edit for paths/settings
# ------------------------------------------------------------------
# Data lives OUTSIDE the git repo. Set DS340_DATA_DIR in your shell, e.g.
#   export DS340_DATA_DIR=$HOME/ds340_data                    (laptop)
#   export DS340_DATA_DIR=/scratch/<acct>/$USER/ds340_data    (Delta)
DATA_DIR    <- Sys.getenv("DS340_DATA_DIR", unset = path.expand("~/ds340_data"))
FLIGHT_DIR  <- file.path(DATA_DIR, "flights")    # Combined_Flights_2019.csv, ...
WEATHER_DIR <- file.path(DATA_DIR, "weather")    # <station>_<year>.csv, one per airport-year
OUT_DIR     <- file.path(DATA_DIR, "processed")  # big intermediate files (not in git)
RESULTS_DIR <- "results"                         # small tables (in git)
FIG_DIR     <- "figures"                         # plots (in git)

# --- study design -------------------------------------------------
YEARS <- 2019:2022

# Study airports -> NOAA Global Hourly station id (USAF + WBAN, no dash).
# Chosen for different weather / traffic regimes:
#   ATL baseline hub (frequent thunderstorms)    ORD snow, wind, thunderstorms
#   DEN snow, high wind                          SFO marine fog / low ceilings
#   JFK dense, congested NE corridor             SEA low cloud / rain
AIRPORTS <- c(ATL = "72219013874",
              ORD = "72530094846",
              DEN = "72565003017",
              SFO = "72494023234",
              JFK = "74486094789",
              SEA = "72793024233")

DELAY_THRESH    <- 15   # "late" = >= 15 min (arrival); "recovered" = next departure < 15 min late
MIN_TURN        <- 30   # keep connections with SCHEDULED turnaround in [MIN_TURN, MAX_TURN] min
MAX_TURN        <- 360  #   (scheduled-only filter: no outcome information used)
WX_MAX_AGE_MIN  <- 90   # reject weather observations older than this at prediction time
TRAFFIC_WIN_MIN <- 60   # count scheduled movements in the next 60 min

# --- modelling ----------------------------------------------------
TRAIN_YEARS <- 2019         # fit on pre-COVID year ...
TEST_YEARS  <- 2020:2022    # ... evaluate on each later year separately
N_BOOT      <- 1000         # day-level bootstrap replicates
SEED        <- 340

dir.create(OUT_DIR,     showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,     showWarnings = FALSE, recursive = TRUE)
