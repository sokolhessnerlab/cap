# Setting up data for AXanalysis.Rmd which uses 'source' to call this script
# This script has to be an .R not .Rmd to run
# This script loads the AX-CPT data and exclusion data, applies the exclusion, deals with missed trials, and creates any variables we need for the analysis (e.g., prior trial-type, prior trial outcome)

# output:
# 1) AX_subID_missTri_totTri.csv (total and missed trials for each participant in phase)

# Kimberly Chiew, University of Denver
# last modified: 3/16/22

# clear out contents of R
rm(list=ls());

# load packages
library('config')
library('lme4')

# configuration
config = config::get()

# load data -- setting up filepaths (using shortcuts in config.yml file)
AX_csv = file.path(config$path$combined, config$AXcsvs$AX_qualtrics);
exclsnPhs1_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs1exclusion); # phase 1 exclusion path
exclsnPhs2_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs2exclusion); # phase 2 exclusion
qualtricsBothPhs_csv = file.path(config$path$combined, config$QUALTRICScsvs$Combined_subID_scored_noDuplicates); # qualtrics responses path
subLevelLong_path = file.path(config$path$Rdata, config$Rdata_files$QualtricsSubLevelLong); # subject-level long format path
subLevelWide_path = file.path(config$path$Rdata, config$Rdata_files$QualtricsSubLevelWide); # subject-level wide format path

# load data -- reading in CSV files
AXQualtrics = read.csv(AX_csv);
excludePhs1 = read.csv(exclsnPhs1_csv); # loads exclusion for phase 1
excludePhs2 = read.csv(exclsnPhs2_csv); # loads exclusion for phase 2
qualtricsBothPhs = read.csv(qualtricsBothPhs_csv); # load qualtrics responses
load(subLevelLong_path); # load subject-level long dataframe
load(subLevelWide_path); # load subject-level wide dataframe

# Remove the extra "X" column present as the first column in datasets
AXQualtrics = AXQualtrics[,(2:ncol(AXQualtrics))];
excludePhs1 = excludePhs1[,(2:ncol(excludePhs1))];
excludePhs2 = excludePhs2[,(2:ncol(excludePhs2))];
qualtricsBothPhs = qualtricsBothPhs[,(4:ncol(qualtricsBothPhs))]; # qualtrics has 3 columns of X variable

# add the PCA SES component one and scaled affective variables to AX datasets
for(s in 1:nrow(subLevelLong)){
  AXQualtrics$sesPCA[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$sesPCA[s];
  AXQualtrics$stai_s_score_scaled[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$stai_s_score_scaled[s];
  AXQualtrics$stai_t_score_scaled[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$stai_t_score_scaled[s];
  AXQualtrics$uclal_score_scaled[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$uclal_score_scaled[s];
  AXQualtrics$pss_score_scaled[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$pss_score_scaled[s];
  AXQualtrics$covq_PAB_q1_personalRisk_scaled[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaled[s];
  AXQualtrics$covq_PAB_q1_personalRisk_scaledNoNA[subLevelLong$subID[s]==AXQualtrics$subID & subLevelLong$phase[s] == AXQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaledNoNA[s];
}

# we are going to exclude some people so lets save the original number of subjects
subNumB4exclusion = unique(AXQualtrics$subID);
nSubB4exclusion = length(subNumB4exclusion);

## Apply the exclusions to the data set by place NAs in trials for excluded participants

# Phase 1: 24 participants excluded
subIDAXPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$axPhs1exclude) & excludePhs1$axPhs1exclude==1]
# note that this code worked but that subIDAXPhs1Exclude was a NULL object afterwards
# 3/16/22: got this to work and produce a string of numbers that are the subjects to exclude


# Qualtrics: 3 participants excluded
subIDqualPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$qualPhs1exclude) & excludePhs1$qualPhs1exclude==1]
# this was directly copied from Hayley's RDM script and also worked and produced a NULL object

# Phase 2: an additional 4 participants excluded (7 excluded but 3 of these were already excluded in Phase 1)
subIDAXPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$axPhs2exclude) & excludePhs2$axPhs2exclude==1]

# Qualtrics: 1 participant excluded based on age response in phase
subIDqualPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$qualPhs2exclude) & excludePhs2$qualPhs2exclude==1]


# column order is funky. RDM stuff is in columns 1-5, 7-14, 17-18, and day, phase, and subID are dispersed throughout.
AXColumns = c(1:4,8,9);


# Qualtrics stuff starts at column 19, or "stai_s_score" until column 71 or "ses_needbasedCollegeAid_recode" and 73 with the sesPCA
# checked and corrected to the same columns as in the RDM data

qualColumns = c(10:62,64);

########################################################

# Put NAs in AX columns for excluded phase 1 and phase 2 participants
AXQualtrics[AXQualtrics$subID %in% subIDAXPhs1Exclude & AXQualtrics$phase==1,AXColumns] = NA; # phase 1
AXQualtrics[AXQualtrics$subID %in% subIDAXPhs2Exclude & AXQualtrics$phase==2,AXColumns] = NA; # phase 2

# Put NAs in Qualtrics columns for excluded phase 1 and phase 2 participants in both gain and loss datasets
# gain task
AXQualtrics[AXQualtrics$subID %in% subIDqualPhs1Exclude & AXQualtrics$phase==1,qualColumns] = NA; # phase 1
AXQualtrics[AXQualtrics$subID %in% subIDqualPhs2Exclude & AXQualtrics$phase==2,qualColumns] = NA; # phase 2

# Participant IDs included in phase 1 AX
Phs1subIDs = excludePhs1$subID[excludePhs1$axPhs1exclud==0];
Phs1nSub = length(Phs1subIDs);

# Participant IDs included in phase 2 AX
Phs2subIDs = excludePhs2$subID[!is.na(excludePhs2$axPhs2exclude) & excludePhs2$axPhs2exclude==0];
Phs2nSub = length(Phs2subIDs);

# Participant IDs included in both phases
BothPhsSubIDs = Phs2subIDs[Phs2subIDs %in% Phs1subIDs];
BothPhsnSub = length(BothPhsSubIDs)

# Prior to exclusion, there were 544 participants. After exclusion, we have AX-CPT data for 520 participants in phase 1 and 350 participants in phase 2, and 333 participants that are included in both phases.

# Missed trials:
# Where participants did not respond, an NA is in place for choice and outcome. We are not removing these trials but will make a note of the number of trials per phase that were missed by each participants.
# At this point in the script, we have NAs for people who are excluded.

### Which trials were missed?

nanAXPhs1 = which(is.na(AXQualtrics$axResponse) & AXQualtrics$subID %in% Phs1subIDs & AXQualtrics$phase ==1) # 1225 missed trials indices phase 1

nanAXPhs2 = which(is.na(AXQualtrics$axResponse) & AXQualtrics$subID %in% Phs2subIDs & AXQualtrics$phase ==2)  # 683 missed trials indices phase 2

nanAX = c(nanAXPhs1, nanAXPhs2);

nanAXPhs1tot = length(nanAXPhs1); #1225 missed trials phase 1
nanAXPhs2tot = length(nanAXPhs2); #683 missed trials phase 2

### Which participants missed trials and how many did each participant miss?

subAXPhs1 = unique(AXQualtrics$subID[nanAXPhs1]); # 268 participants missed at least one trial
subAXPhs2 = unique(AXQualtrics$subID[nanAXPhs2]); # 160 participants missed at least one trial

# Create a dataframe that stores subject IDs, missed AX trials phase 1, missed AX trials phase 2, total AX trials phase 1, total AX trials phase 2.
subID_missTri_totTri = as.data.frame(matrix(data=NA, nrow = nSubB4exclusion, ncol=5, dimnames = list(c(NULL), c("subID", "missAXTriPhs1", "missAXTriPhs2","totalAXTriPhs1", "totalAXTriPhs2"))));

