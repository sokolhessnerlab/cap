# CAP study: Add scored qualtrics data to AX and RDM clean datasets
# Hayley Brooks
# Sokol-Hessner Lab
# University of Denver
# left off here 5/11

# output:
# 1) AXallClean_Qualtrics.csv
# 2) RDMallClean_Qualtrics.csv

# clean environment
rm(list=ls());

# load data
scoredQualtrics = read.csv("/Volumes/CAP/data/combinedData/QualtricsCombined_subID_scored_noDuplicates.csv"); # scored qualtrics
rdmDF = read.csv("/Volumes/CAP/data/combinedData/RDMallClean.csv"); # RDM data
axDF = read.csv("/Volumes/CAP/data/combinedData/AXallClean.csv"); # AX data   

subNum = unique(rdmDF$subID);
nSub = length(unique(rdmDF$subID)); #544


# we can pair down scoredQualtrics to make it easier to work with, lets keep the columns we want
# column names we want to add here
# stai_s_score (column 33)
# stai_t_score (column 56)
# pss_score (column 67)
# pss_stressedToday (column 70)
# uclal_score (column 91)
# columns 92:140

qualtricsDF = scoredQualtrics[,c(33,56,67,70,91:140)]; # keep only the columns we listed above

# merge the data frames by sub ID and phase 
# this will also get rid of the redundant columns (subID and phase)
AXallClean_Qualtrics = merge(axDF,qualtricsDF,by=c("subID","phase"), all.x = T); # AX
RDMallClean_Qualtrics = merge(rdmDF,qualtricsDF,by=c("subID","phase"), all.x = T);  # RDM


# save the output
write.csv(file = "/Volumes/CAP/data/combinedData/AXallClean_Qualtrics.csv", AXallClean_Qualtrics);
write.csv(file = "/Volumes/CAP/data/combinedData/RDMallClean_Qualtrics.csv", RDMallClean_Qualtrics);

