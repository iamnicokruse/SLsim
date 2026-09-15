#!/bin/bash
#SBATCH --job-name=SLsim_test
#SBATCH --time=96:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=32G
#SBATCH --partition=pub23

# Output/Error
#SBATCH --output=sim_test_%j.out
#SBATCH --error=sim_test_%j.err

###############################################################################
# Projekt starten
###############################################################################

echo "=== Testjob gestartet auf $(hostname) um $(date) ==="

cd ~/SLsim || { echo "Projektordner nicht gefunden!"; exit 1; }

# Ordner erzeugen
mkdir -p results
mkdir -p logs

###############################################################################
# Module laden
###############################################################################

module load R

###############################################################################
# Threads korrekt setzen
###############################################################################

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

echo "Working directory: $(pwd)"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "OMP threads: $OMP_NUM_THREADS"

###############################################################################
# R Script ausführen
###############################################################################

echo "=== Starte Testlauf ==="

# block_id = 1 → 10 Simulationen
Rscript SLsim_master.R 1

###############################################################################
# Ende
###############################################################################

echo "=== Testjob beendet um $(date) ==="