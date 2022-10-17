#!/bin/sh
#
#$ -l h_rt=24:00:00
#$ -M *
#$ -cwd
#$ -e error
#$ -o output
#$ -S /bin/sh
#$ -l h_vmem=24G


./Roche_QPCR_Preprocessing_and_analysis.bash
