#!/bin/bash
#instal bidskit via Docker
#for subj in 01 , #02 03 04 05 06 07 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
  #do
docker run -it -v /home/REWOD/SOURCE/ jmtyszka/bidskit -d /REWOD


#If you're running bidskit from the shell you can either run bidskit without arguments from within the dataset root

# cd /PATH_TO_YOUR_DATASET_FOLDER/
# bidskit
#or from another folder by specifying the BIDS dataset directory

# bidskit -d /PATH_TO_YOUR_DATASET_FOLDER/ 