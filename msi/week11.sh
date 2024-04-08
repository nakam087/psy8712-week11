#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=16
#SBSTCH --mem=16gb
#SBATCH -t 12:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=nakam087@umn.edu
#SBATCH -p amdsmall
cd ~/out
module load R/4.3.0-openblas
Rscript week11-cluster.R
