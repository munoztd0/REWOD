#!/bin/bash
home=$(eval echo ~$user);

task="PIT"
mod="eff"
GLM="GLM-03"
threshold="0.01"
codeDir="${home}/REWOD/DERIVATIVES/PREPROC/CANONICALS/"

echo ${codeDir}
cd ${codeDir}

#fslmaths harvardoxford-subcortical_prob_Left_Amygdala.nii -thr 50 -bin AMY_LEFT.nii



gunzip *LEFT.nii.gz
