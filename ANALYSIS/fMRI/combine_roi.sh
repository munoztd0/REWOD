#!/bin/bash
home=$(eval echo ~$user);

task="PIT"
mod="eff"
GLM="GLM-03"
threshold="0.01"
codeDir="${home}/REWOD/DERIVATIVES/ANALYSIS/${task}/ROI/${threshold}/${GLM}/${mod}/"

echo ${codeDir}
cd ${codeDir}

fslmaths CAUD_VENTR_LEFT.nii -add CAUD_VENTR_RIGHT.nii combined_CAUD_VENTR.nii

fslmaths dlPFC_LEFT.nii -add dlPFC_RIGHT.nii combined_dlPFC.nii

fslmaths FRONTAL_LEFT.nii -add FRONTAL_RIGHT.nii combined_FRONTAL.nii

fslmaths OFC_LEFT.nii -add OFC_RIGHT.nii combined_OFC.nii

fslmaths pINS_LEFT.nii -add pINS_RIGHT.nii combined_pINS.nii

fslmaths SUBCAL_LEFT.nii -add SUBCAL_RIGHT.nii combined_SUBCAL.nii

fslmaths vmPFC_LEFT.nii -add vmPFC_RIGHT.nii combined_vmPFC.nii

mkdir ROIs

gunzip combined*

mv *combined /ROIs
