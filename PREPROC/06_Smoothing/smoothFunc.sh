#!/bin/bash

# pull in the subject we should be working on
subjID=$1

#choose task OR runID=$2
taskID=$2


#new directory with final preprocesssed bold files
outDir=~/REWOD/DERIVATIVES/PREPROC/sub-${subjID}/func/
funcImage=~/REWOD/DATA/DERIVATIVES/PREPROC/sub-${subjID}/ses-second/func/task-${taskID}.ica/filtered_func_data_clean_unwarped_Coreg

#FWHM = sigma*sqrt(8*ln(2))
smoothKern=1.69865806013 # to smooth 4 mm


echo "Smoothing Subject ${subjID} Session  at $(date +"%T")"

#kernel gauss takes the sigma (not the pixel FWHM) = sigma*2.3548
fslmaths ${funcImage} -kernel gauss ${smoothKern} -fmean ${outDir}sub-${subjID}_task-${taskID}_run-01_smoothBold


# unzip for use in SPM
echo "Expanding Subject ${subjID} Session 2 at $(date +"%T")"
gunzip -f ${outDir}sub-${subjID}_task-${taskID}_run-01_smoothBold.nii.gz
echo "Done expanding Subject ${subjID} Session 2 at $(date +"%T")"
