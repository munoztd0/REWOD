home=$(eval echo ~$user)

#SAVED is where I have the dicom files uploaded
for subj in 1 2 3 4 5 6 7 9
  do
  matlab dicomanon  ${home}/REWOD/SAVED/RAW/0${subj}/second/${subj}_MRI/dcm*/*
done

#check for dicomanon doc for more info on what to keep or not
for subj in 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25 26
  do
  matlab dicomanon ${home}/REWOD/SAVED/RAW/${subj}/second/${subj}_MRI/dcm*/*
done
