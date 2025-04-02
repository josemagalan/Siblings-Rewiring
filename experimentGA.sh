#!/bin/bash
#SBATCH --job-name=exp_paralelo
#SBATCH --output=logs/output_%A_%a.log
#SBATCH --error=logs/error_%A_%a.log
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --chdir=./
#SBATCH --time=180:00
#SBATCH --array=1-180# Lanza 180 jobs en paralelo

module load OPENMPI/4.1.6
module load gcc/12.3.0
module load R/4.2.2

echo "Iniciando tarea $SLURM_ARRAY_TASK_ID en: $(date)"

time Rscript GA_experiment.R

echo "Finalizando tarea $SLURM_ARRAY_TASK_ID en: $(date)"