# A brief summary of the process and results of AXexclusion.R script

There is one criteria for excluding participants based on the AX-CPT data which is that participants must perform (globally) at or above 60% accuracy.

This script calculates the accuracy (or percent correct) for each participant in each phase and then uses that criteria to determine exclusion.

- In phase 1, there are 22 participants who performed below 60% accuracy
- In phase 2, there are 6 participants who performed below 60% accuracy

Percent correct information is stored in AXqa.csv

2 participants are excluded from phase 1 and phase 2: subIDs are 176 and 543

The output is axExclusion.csv and has 3 columns:
1) SubID: 1-544\
2) Phase1exclude: 1 = yes, exclude; 0 = no, keep
3) Phase2exclude: 1 = yes, exclude; 0 = no, keep
NA means they did not complete the phase (will only show up in phase 2)
