### Brief summary on process of add_sesPCA_affectVar_AX.R

- The point of this script is to take the individual-level dataframe (long format) which has the ses PCA component, the scaled affective scores (pss, uclal, stait) and the covid risk variables (scaled and scaled with no NAs) and add these to the AX data
- We take the AXqualtrics dataframe and add the variables listed above to it
- the output is "/Volumes/CAP/Rdata/axQualtricsPCA.Rdata"
