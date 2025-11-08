
# Sibling Rewiring Project

This repository contains the code, data, and experimental framework developed for the study:

**"Optimizing Classroom Assignments to Minimize Epidemiological Risk: The Sibling Rewiring Problem"**

## Overview

The Sibling Rewiring Problem (SRP) is a novel multi-objective optimization framework designed to improve classroom group assignments in educational institutions. By considering sibling relationships—which introduce latent connections between otherwise isolated classroom groups—the SRP aims to reduce potential transmission pathways of infectious diseases (such as COVID-19) while ensuring equitable group sizes.

This repository provides an implementation of several strategies to address the SRP, including:

- Baseline random assignment
- "bubble" grouping
- A custom heuristic strategy that assigns siblings to the same classroom line
- Simulated Annealing (SA) metaheuristic
- NSGA-II multi-objective genetic algorithm (GA)

## Objectives

The Sibling Rewiring Problem simultaneously optimizes two conflicting objectives:

1. **Maximize network fragmentation** (i.e., increase the number of connected components in the classroom interaction network) to reduce epidemiological risk.
2. **Minimize variance in component sizes**, promoting fairness in exposure risk distribution.

## Repository Structure

- `datasets/`: Contains input CSV files with student enrollment data.
- `results/`: Stores output files, including assignment results and visualizations.
- `analysis/`: Includes scripts and notebooks for analyzing the results.
- `robustness/`: Includes scripts and notebooks for creating, run the experiments and analyzing the results of the robustness appendix.
- `robustness/datasetsRobustness`: Contains input CSV files with student enrollment data for the robustness analysis.
- `GA_experiment.R`: Script to run experiments using Genetic Algorithms.
- `SAexperiment.R`: Script to run experiments using Simulated Annealing.
- `heuristicExperiment.R`: Script to run heuristic-based and basic assignment experiments.
- `datasetCreator.R`: Tool to create synthetic datasets.
- `analysis.R`: Script for comprehensive analysis of experimental results.
- `graphFigures.R`: Functions for generating network visualizations.
- `nivelacion.R`: Contains functions related to balancing or leveling assignments.


## Getting Started

### Requirements
This repository uses R and the following libraries:

```R
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)
library(viridis)
```

Make sure you have `R >= 4.0` installed and all dependencies available.

### Running Experiments
To reproduce the experiments:

1. Generate or download the synthetic datasets into the `datasets/` folder.
2. Execute the scripts for initial, bubble, heuristic, SA, and GA strategies.
3. Aggregated results and graphs will be stored in `results/` and `analysis/`.

### Output Files
- `.jpg` / `.pdf`: Network visualizations and statistical plots.
- `.csv`: Summary of results, Pareto fronts, and performance metrics.
- `.rds`: Intermediate assignment results.

## Citation
If you use this code or data in your research, please cite:

Paper under review

## License
This project is released under the [MIT License](LICENSE).
