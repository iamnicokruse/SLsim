#!/bin/bash
#SBATCH --job-name=sim_future
#SBATCH --output=sim_future_%j.out
#SBATCH --error=sim_future_%j.err
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=25
#SBATCH --mem=32G
#SBATCH --partition=pub23

# --- Optional: Modul laden, falls nötig ---
module load R

# --- Ausgabe von Systeminformationen ---
echo "=== Job gestartet auf $(hostname) um $(date) ==="
echo "Verwendete Kerne: $SLURM_CPUS_PER_TASK"

# --- R-Skript ausführen ---
Rscript future_demo.R

echo "=== Job beendet um $(date) ==="


echo "=== SLURM ACCOUNT INFO ==="
echo "User:     $USER"
echo "Account:  $SLURM_JOB_ACCOUNT"
echo "Partition:$SLURM_JOB_PARTITION"
echo "Job ID:   $SLURM_JOB_ID"
echo "=========================="