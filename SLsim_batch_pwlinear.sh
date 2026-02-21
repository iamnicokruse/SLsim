#!/bin/bash
#SBATCH --job-name=SLsim_pwlinear
#SBATCH --time=60:00:00
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --array=1-10
#SBATCH --mem=32G
#SBATCH --partition=pub23

# Output/Error sauber ins Hauptverzeichnis (damit Job nicht sofort crasht)
#SBATCH --output=sim_%A_%a.out
#SBATCH --error=sim_%A_%a.err

###############################################################################
# Projekt starten
###############################################################################

echo "=== Job gestartet auf $(hostname) um $(date) ==="

# Projektordner setzen
cd ~/SLsim || { echo "Projektordner nicht gefunden!"; exit 1; }

# Ordner erzeugen (müssen existieren bevor R schreibt)
mkdir -p results
mkdir -p logs

# Optional: Logs nachträglich verschieben
# (damit du sie trotzdem im logs/ Ordner hast)

###############################################################################
# Module laden
###############################################################################

module load R

###############################################################################
# Threads korrekt setzen
###############################################################################

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export MKL_NUM_THREADS=$SLURM_CPUS_PER_TASK
export OPENBLAS_NUM_THREADS=1

echo "Working directory: $(pwd)"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "OMP threads: $OMP_NUM_THREADS"

###############################################################################
# R Script ausführen
###############################################################################

echo "=== Starte R Simulation ==="

Rscript SLsim_master_pwlinear.R $SLURM_ARRAY_TASK_ID

###############################################################################
# Logs einsortieren
###############################################################################

mv sim_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out logs/
mv sim_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.err logs/

echo "=== Job beendet um $(date) ==="
