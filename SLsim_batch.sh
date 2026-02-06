#!/bin/bash
#SBATCH --job-name=SLsim
#SBATCH --output=logs/sim_%j.out
#SBATCH --error=logs/sim_%j.err
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=32G
#SBATCH --partition=pub23

# Projektordner setzen
cd ~/SLsim || exit 1

# Ordner erzeugen
mkdir -p results
mkdir -p logs

# Module laden
module load R

# Threads korrekt setzen
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK

echo "=== Job gestartet auf $(hostname) um $(date) ==="
echo "Working directory: $(pwd)"
echo "CPUs: $SLURM_CPUS_PER_TASK"

# Run
Rscript SLsim_master.R

echo "=== Job beendet um $(date) ==="
