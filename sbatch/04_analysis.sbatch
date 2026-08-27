#!/bin/bash
#
#SBATCH --job-name=alc_analysis
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=2
#SBATCH --time=1:00:00
#SBATCH --mem=16GB
#SBATCH --mail-type=END
#SBATCH --mail-user=tc3421@nyu.edu
#SBATCH --output=slurm_analysis_%j.out

module purge
module load r/4.5.1

cd /scratch/tc3421/Politicization_paper   # EDIT if your project lives elsewhere
Rscript scripts/04_analysis_and_figures.R
