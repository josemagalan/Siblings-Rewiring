#!/bin/bash

# SLURM job script to launch multiple parallel simulated annealing optimization experiments using R

#SBATCH --job-name=exp_paralelo             # Job name
#SBATCH --output=logs/output_%A_%a.log      # Standard output file (one per array task)
#SBATCH --error=logs/error_%A_%a.log        # Standard error file (one per array task)
#SBATCH --nodes=1                           # Number of nodes
#SBATCH --ntasks-per-node=1                 # One task per node
#SBATCH --cpus-per-task=4                   # Number of CPUs per task
#SBATCH --chdir=./                          # Set working directory
#SBATCH --time=180:00                       # Time limit (hh:mm)
#SBATCH --array=1-180                       # Launch 180 parallel jobs (array tasks from 1 to 180)

# Load necessary modules
module load OPENMPI/4.1.6                   # Load OpenMPI module
module load gcc/12.3.0                      # Load GCC compiler
module load R/4.2.2                         # Load R environment

# Log start time and task ID
echo "Starting task $SLURM_ARRAY_TASK_ID at: $(date)"

# Execute the R script that performs the optimization experiment
time Rscript experiment.R

# Log end time
echo "Ending task $SLURM_ARRAY_TASK_ID at: $(date)"
