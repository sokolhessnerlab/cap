# Combine CAP exclusion matrices so that we have one for phase 1 (RDM, AX, Qualtrics) and phase 2 (RDM, AX, Qualtrics)
# Hayley Brooks
# Sokol-Hessner Lab
# University of Denver

# output
# 1) allExclusionPhase1.csv
# 2) allExclusionPhase2.csv


# clear environment
rm(list = ls());

# load files
rdmexclusion = read.csv("/Volumes/CAP/data/combinedData/rdmExclusion.csv");          # rdm exclusion
axexclusion = read.csv("/Volumes/CAP/data/combinedData/axExclusion.csv");            # ax exclusion
qualexclusion = read.csv("/Volumes/CAP/data/combinedData/qualtricsExclusion.csv");   # qualtrics exclusion
dayInfo = read.csv("/Volumes/CAP/data/combinedData/phase1_participant.csv");         # participant info to get day (days are identical across phases so we just need phase 1)

nSub = nrow(rdmexclusion); # number of participants

# combine 
allExclusionPhase1 = cbind(1:nSub,dayInfo[,2], rdmexclusion[3], axexclusion[3], qualexclusion[3]); # phase 1
allExclusionPhase2 = cbind(1:nSub,dayInfo[,2], rdmexclusion[4], axexclusion[4], qualexclusion[4]); # phase 2

# set the column names
ColNamesPhase1 = c("subID", "day", "rdmPhs1exclude", "axPhs1exclude", "qualPhs1exclude"); 
ColNamesPhase2 = c("subID", "day", "rdmPhs2exclude", "axPhs2exclude", "qualPhs2exclude"); 

# rename columns
colnames(allExclusionPhase1) = ColNamesPhase1; 
colnames(allExclusionPhase2) = ColNamesPhase2; 



# save as .csv files
write.csv(file = "/Volumes/CAP/data/combinedData/allExclusionPhase1.csv", allExclusionPhase1);
write.csv(file = "/Volumes/CAP/data/combinedData/allExclusionPhase2.csv", allExclusionPhase2);
