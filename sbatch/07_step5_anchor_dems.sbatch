#!/bin/bash
#
#SBATCH --job-name=step5_anchors
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=2:00:00
#SBATCH --mem=32GB
#SBATCH --mail-type=END
#SBATCH --mail-user=tc3421@nyu.edu
#SBATCH --output=slurm_step5anchors_%j.out

module purge
module load r/4.5.1

cd /scratch/tc3421/Politicization_paper
Rscript scripts/07_step5_anchor_dems.R
