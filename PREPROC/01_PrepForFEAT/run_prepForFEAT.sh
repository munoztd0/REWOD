#!/bin/bash

codeDir=/home/REWOD/ANALYSIS/fsl_scripts/clean_preproc/01_PrepForFEAT/

anatomicalScript=${codeDir}prepAnatomical.sh
functionalScript=${codeDir}prepFunctional.sh

# Loop over subjects
 for subjectID in 01 02 03 04 05 06 07 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26

do
	# work on each subject's anatomical scans
	qsub -o /home/REWOD/ClusterOutput -j oe -l walltime=1:00:00,pmem=6GB -M david.munoz@etu.unige.ch -m e -l nodes=1 -q queue1 -N Anat_prepFEAT_sub-${subjectID} -F "${subjectID}" ${anatomicalScript}
##	qsub -o ~/ClusterOutput -j oe -l walltime=1:00:00 -M evapool@caltech.edu -m e -l nodes=1 -q batch -N prepFEAT_Subject_${subj} -F "${subj}" ${anatomicalScript}

	# prep for each task's data
  for taskID in PIT hedonic

	do
			qsub -o /home/REWOD/ClusterOutput -j oe -l walltime=0:40:00,pmem=6GB -M david.munoz@etu.unige.ch -m e -l nodes=1 -q queue1 -N Func_prepFEAT_sub-${subjectID}_task-${taskID} -F "${subjectID} ${taskID}" ${functionalScript}
#		qsub -o ~/ClusterOutput -j oe -l walltime=0:10:00 -M evapool@caltech.edu -m e -l nodes=1 -q batch -N prepFEAT_Subject_${subj}_${runID} -F "${subj} ${runID}" ${functionalScript}

	done
done
