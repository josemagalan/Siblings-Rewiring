#!/bin/bash

# SLURM job script to run multiple parallel optimization experiments 
# using a custom implementation of the NSGA-II genetic algorithm in R

#SBATCH --job-name=exp_paralelo             # Job name
#SBATCH --output=logs/output_%A_%a.log      # Standard output file (one per array task)
#SBATCH --error=logs/error_%A_%a.log        # Standard error file (one per array task)
#SBATCH --nodes=1                           # Number of nodes
#SBATCH --ntasks-per-node=1                 # One task per node
#SBATCH --cpus-per-task=4                   # Number of CPUs per task
#SBATCH --chdir=./                          # Set working directory
#SBATCH --time=180:00                       # Time limit (hh:mm)
#SBATCH --array=1-180                       # Launch 180 parallel jobs (array tasks from 1 to 180)

# Load required modules
module load OPENMPI/4.1.6                   # Load OpenMPI for parallel execution
module load gcc/12.3.0                      # Load GCC compiler
module load R/4.2.2                         # Load R environment

# Log the start of the task
echo "Starting task $SLURM_ARRAY_TASK_ID at: $(date)"

# Run the R script implementing the NSGA-II genetic algorithm
time Rscript GA_experiment.R

# Log the end of the task
echo "Ending task $SLURM_ARRAY_TASK_ID at: $(date)"
