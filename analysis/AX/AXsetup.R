# Setting up data for AXanalysis.Rmd which uses 'source' to call this script
# This script has to be an .R not .Rmd to run
# This script loads the AX-CPT data and exclusion data, applies the exclusion, deals with missed trials, and creates any variables we need for the analysis (e.g., prior trial-type, prior trial outcome)

# output:
# 1) AX_subID_missTri_totTri.csv (total and missed trials for each participant in phase)

# Kimberly Chiew, University of Denver
# last modified: 2/2/22

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

# Phase 1: not sure how many AX participants excluded
subIDAXPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$AXPhs1exclude) & excludePhs1$AXPhs1exclude==1]
# note that this code worked but that subIDAXPhs1Exclude was a NULL object afterwards


# Qualtrics: 3 participants excluded
subIDqualPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$qualPhs1exclude) & excludePhs1$qualPhs1exclude==1]
# this was directly copied from Hayley's RDM script and also worked and produced a NULL object

# Phase 2: not sure how many AX participants excluded
subIDAXPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$AXPhs2exclude) & excludePhs2$AXPhs2exclude==1]

# Qualtrics: 1 participant excluded based on age response in phase
subIDqualPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$qualPhs2exclude) & excludePhs2$qualPhs2exclude==1]


########################################################

# it is not yet clear to me from this how task-specific exclusion criteria are applied
# e.g., the information in rdmExclusion.csv for Hayley (RDMsetup.R did not mention this csv) or
# axExclusion for my data (which is participants with < 60% accurate performance)
# will need to go back to this and include task-specific exclusions

