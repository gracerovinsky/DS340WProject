#!/bin/bash
# Download NOAA Global Hourly CSVs for every study airport and year.
# Run on your laptop (or a Delta login node), then upload to $DS340_DATA_DIR/weather/.
# Usage: DS340_DATA_DIR=$HOME/ds340_data bash scripts/00_download_weather.sh
DATA_DIR=${DS340_DATA_DIR:-$HOME/ds340_data}
mkdir -p "$DATA_DIR/weather"
STATIONS="72219013874 72530094846 72565003017 72494023234 74486094789 72793024233"   # ATL ORD DEN SFO JFK SEA
for yr in 2019 2020 2021 2022; do
  for st in $STATIONS; do
    out="$DATA_DIR/weather/${st}_${yr}.csv"
    [ -s "$out" ] && { echo "have $out"; continue; }
    url="https://noaa-global-hourly-pds.s3.amazonaws.com/${yr}/${st}.csv"
    echo "GET $url"; curl -fsSL "$url" -o "$out" || { echo "FAILED: $url"; rm -f "$out"; }
  done
done
ls -l "$DATA_DIR/weather"
