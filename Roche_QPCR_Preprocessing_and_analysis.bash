#!/bin/bash

#SBATCH --mail-type=ALL
#SBATCH --mail-user=*@*
#SBATCH --time=1-0
#SBATCH --job-name=qPCR
#SBATCH --mem-per-cpu=2000


 R CMD BATCH  --vanilla --slave /Users/.../.../Roche_QPCR_Preprocessing_and_analysis.R Roche_qPCR_Script


