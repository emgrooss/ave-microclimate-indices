#!/bin/bash
#SBATCH -p medium
#SBATCH -n 1
#SBATCH -t 5:00:00
#SBATCH -c 4
#SBATCH -o ./output_log/microclimate_sim.out
#SBATCH -e ./error_log/microclimate_sim.err
#SBATCH --mem=48G  
#SBATCH --mail-user=emgrooss@gmail.com
#SBATCH --mail-type=ALL           

# Load modules and spack packages
module load miniforge3
eval "$(conda shell.bash hook)"
conda activate /scratch/users/u15397/r_env

# Run the R script
Rscript scripts/2-microclimate_simulation.R