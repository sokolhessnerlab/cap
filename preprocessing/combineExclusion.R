# Combine CAP exclusion matrices so that we have one for phase 1 (RDM, AX, Qualtrics) and phase 2 (RDM, AX, Qualtrics)
# Hayley Brooks
# Sokol-Hessner Lab
# University of Denver

# output
# 1) allExclusionPhase1.csv
# 2) allExclusionPhase2.csv


# clear environment
rm(list = ls());

# configuration
config = config::get();

# load files
rdmExclusionPath = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_exclusion)
rdmExclusion = read.csv(rdmExclusionPath);
#rdmexclusion = read.csv("/Volumes/CAP/data/combinedData/rdmExclusion.csv");          # rdm exclusion

axexclusionPath = file.path(config$path$combined, config$EXCLUSIONcsvs$AX_exclusion)
axexclusion = read.csv(axexclusionPath);
#axexclusion = read.csv("/Volumes/CAP/data/combinedData/axExclusion.csv");            # ax exclusion

qualexclusionPath = file.path(config$path$combined, config$EXCLUSIONcsvs$QUALTRICS_exclusion)
qualexclusion = read.csv(qualexclusionPath);
#qualexclusion = read.csv("/Volumes/CAP/data/combinedData/qualtricsExclusion.csv");   # qualtrics exclusion

dayInfoPath = file.path(config$path$combined, config$SUBcsvs$phase1_participant);
dayInfo = read.csv(dayInfoPath);
#dayInfo = read.csv("/Volumes/CAP/data/combinedData/phase1_participant.csv");         # participant info to get day (days are identical across phases so we just need phase 1)

nSub = nrow(rdmExclusion); # number of participants

# combine 
allExclusionPhase1 = cbind(1:nSub,dayInfo[,2], rdmExclusion[3], axexclusion[3], qualexclusion[3]); # phase 1
allExclusionPhase2 = cbind(1:nSub,dayInfo[,2], rdmExclusion[4], axexclusion[4], qualexclusion[4]); # phase 2

# set the column names
ColNamesPhase1 = c("subID", "day", "rdmPhs1exclude", "axPhs1exclude", "qualPhs1exclude"); 
ColNamesPhase2 = c("subID", "day", "rdmPhs2exclude", "axPhs2exclude", "qualPhs2exclude"); 

# rename columns
colnames(allExclusionPhase1) = ColNamesPhase1; 
colnames(allExclusionPhase2) = ColNamesPhase2; 

# sub 30 and 373 are excluded across the study - take care of that here:
allExclusionPhase1[allExclusionPhase1$subID==30,3:5]=1;
allExclusionPhase2[allExclusionPhase2$subID==30,3:5]=1;

allExclusionPhase1[allExclusionPhase1$subID==373,3:5]=1;


# save as .csv files
allExclusionPhs1Path = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs1exclusion);
write.csv(allExclusionPhase1, file=allExclusionPhs1Path)
#write.csv(file = "/Volumes/CAP/data/combinedData/allExclusionPhase1.csv", allExclusionPhase1);

allExclusionPhs2Path = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs2exclusion);
write.csv(allExclusionPhase2, file=allExclusionPhs2Path)
#write.csv(file = "/Volumes/CAP/data/combinedData/allExclusionPhase2.csv", allExclusionPhase2);
