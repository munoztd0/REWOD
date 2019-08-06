#!/bin/bash
home=$(eval echo ~$user)

for subj in 1 2 3 4 5 6 7 9
  do
  cp -r ${home}/REWOD/SAVED/RAW/0${subj}/second/${subj}_MRI/6*/*/* ${home}/REWOD/SOURCE/0${subj}/
done

for subj in 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25 26
  do
  cp -r ${home}/REWOD/SAVED/RAW/${subj}/second/${subj}_MRI/6*/*/* ${home}/REWOD/SOURCE/${subj}/
done
