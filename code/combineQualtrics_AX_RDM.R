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

AXallClean_Qualtrics = as.data.frame(matrix(data=NA))# , ncol = (ncol(axDF) + ncol(qualtricsDF)))
RDMallClean_Qualtrics = as.data.frame(matrix(data=NA))

for (s in 1:nSub) {
  
  # get a single subject
#  singleSubRDM = rdmDF[rdmDF$subID==subNum[s],]
  singleSubAX = axDF[axDF$subID==subNum[s],]
  singleSubQual = qualtricsDF[qualtricsDF$subID==subNum[s],]
  
  #rdmQualtricsSingleSub = cbind(singleSubRDM,singleSubQual)
  axQualtricsSingleSub = cbind(singleSubAX,singleSubQual)
  
  AXallClean_Qualtrics = cbind(AXallClean_Qualtrics,axQualtricsSingleSub)
  #RDMallClean_Qualtrics = cbind(RDMallClean_Qualtrics,rdmQualtricsSingleSub)
  
  
}


