#!/bin/bash
#SBATCH --job-name=ds340_pipeline
#SBATCH --account=YOUR_DELTA_ACCOUNT_cpu     # <-- edit (find with: accounts)
#SBATCH --partition=cpu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=logs/pipeline_%j.out

# Run from the repo root:  mkdir -p logs && sbatch slurm/run_pipeline.sbatch
module load r                      # check exact name with: module avail r
export DS340_DATA_DIR=/scratch/YOUR_ACCOUNT/$USER/ds340_data   # <-- edit

echo "Start: $(date)"; echo "Node: $(hostname)  CPUs: $SLURM_CPUS_PER_TASK  Mem: 96G"
set -e
Rscript scripts/01_build_flights.R
Rscript scripts/02_clean_weather.R
Rscript scripts/03_build_connections.R
Rscript scripts/04_models.R
echo "End: $(date)"
sacct -j $SLURM_JOB_ID --format=JobID,Elapsed,MaxRSS,AllocCPUS,State   # record this for the report's compute section
