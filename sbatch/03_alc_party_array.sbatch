#!/bin/bash
#
#SBATCH --job-name=alc_party
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=2
#SBATCH --time=3:30:00
#SBATCH --mem=16GB
#SBATCH --mail-type=END
#SBATCH --mail-user=tc3421@nyu.edu
#SBATCH --output=slurm_party_%A_%a.out
#SBATCH --array=1-133%20

module purge
module load r/4.5.1

cd /scratch/tc3421/Politicization_paper
Rscript scripts/03_alc_party_task.R
