#!/bin/bash
#
#SBATCH --job-name=appendixG
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=2
#SBATCH --time=10:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=END
#SBATCH --mail-user=tc3421@nyu.edu
#SBATCH --output=slurm_appendixG_%j.out

module purge
module load r/4.5.1

cd /scratch/tc3421/Politicization_paper
Rscript scripts/AppendixG_outlet_analysis.R
