#!/bin/bash
#SBATCH --job-name=SLsim
#SBATCH --time=60:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem=32G
#SBATCH --partition=pub23

# Output/Error sauber ins Hauptverzeichnis (damit Job nicht sofort crasht)
#SBATCH --output=sim_%j.out
#SBATCH --error=sim_%j.err

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

echo "Working directory: $(pwd)"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "OMP threads: $OMP_NUM_THREADS"

###############################################################################
# R Script ausführen
###############################################################################

echo "=== Starte R Simulation ==="

Rscript SLsim_master.R

###############################################################################
# Logs einsortieren
###############################################################################

mv sim_${SLURM_JOB_ID}.out logs/ 2>/dev/null
mv sim_${SLURM_JOB_ID}.err logs/ 2>/dev/null

echo "=== Job beendet um $(date) ==="
