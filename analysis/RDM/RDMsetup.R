# Setting up data for RDManalysis.Rmd which uses 'source' to call this script
# This script has to be an .R not .Rmd to run
# This script loads the risky decision making data and the exclusion data, applies the exclusion, deals with missed trials, and creates any variables we need for the analysis (e.g. past outcome)


# output:
# 1) RDM_subID_missTri_totTri.csv (total and missed trials for each participant in phase for gain and loss datasets)

# Hayley Brooks, University of Denver


# load packages
library('config')
library("lme4");

# configuration
config = config::get()

# load data
rdmGain_csv = file.path(config$path$combined, config$RDMcsvs$RDMgain_qualtrics); # gain only task path
rdmLoss_csv = file.path(config$path$combined, config$RDMcsvs$RDMloss_qualtrics); # loss only task path
rdmBoth_csv = file.path(config$path$combined, config$RDMcsvs$RDMbothTasks_qualtrics); # Both tasks path
exclsnPhs1_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs1exclusion); # phase 1 exclusion path
exclsnPhs2_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs2exclusion); # phase 2 exclusion path
qualtricsBothPhs_csv = file.path(config$path$combined, config$QUALTRICScsvs$Combined_subID_scored_noDuplicates); # qualtrics responses path
subLevelLong_path = file.path(config$path$Rdata, config$Rdata_files$QualtricsSubLevelLong); # subject-level long format path
subLevelWide_path = file.path(config$path$Rdata, config$Rdata_files$QualtricsSubLevelWide); # subject-level wide format path
PBI_path = file.path(config$path$combined, 'CAP_AX_PBI_combined.csv')

cat('Loading decision-making data files...\n')
rdmGainQualtrics = read.csv(rdmGain_csv); # loads gain only RDM + Qualtrics data both phases (takes several seconds)
rdmLossQualtrics = read.csv(rdmLoss_csv); # loads loss only RDM + Qualtrics data both phases (takes several seconds)
rdmBothQualtrics = read.csv(rdmBoth_csv); # loads both tasks + Qualtrics data both phases (takes several seconds)
excludePhs1 = read.csv(exclsnPhs1_csv); # loads exclusion for phase 1
excludePhs2 = read.csv(exclsnPhs2_csv); # loads exclusion for phase 2
qualtricsBothPhs = read.csv(qualtricsBothPhs_csv); # load qualtrics responses
load(subLevelLong_path); # load subject-level long dataframe
load(subLevelWide_path); # load subject-level wide dataframe
pbi_data = read.csv(PBI_path);
cat('Data files loaded.\n')
cat('Preparing additional variables...\n')

# Remove the extra "X" column present as the first column in datasets
rdmGainQualtrics = rdmGainQualtrics[,(2:ncol(rdmGainQualtrics))];
rdmLossQualtrics = rdmLossQualtrics[,(2:ncol(rdmLossQualtrics))];
rdmBothQualtrics = rdmBothQualtrics[,(2:ncol(rdmBothQualtrics))];
excludePhs1 = excludePhs1[,(2:ncol(excludePhs1))];
excludePhs2 = excludePhs2[,(2:ncol(excludePhs2))];
qualtricsBothPhs = qualtricsBothPhs[,(4:ncol(qualtricsBothPhs))]; # qualtrics has 3 columns of X variable


# Add PBI data to subLevelWide variable.
# initialize columns
subLevelWide$PBI_phase1_ERROR = NA;
subLevelWide$PBI_phase2_ERROR = NA;
subLevelWide$PBI_phase1_RT = NA;
subLevelWide$PBI_phase2_RT = NA;

subLevelLong$PBI_ERROR = NA;
subLevelLong$PBI_RT = NA;

# Check for subject matches and input data where appropriate
for(s in 1:length(subLevelWide$subID)){
  index = which(pbi_data$subID == subLevelWide$subID[s]);
  if (length(index) == 0){
    next # if no match (i.e. missing AX data), proceed to next person
  } else { # if match, include the data
    subLevelWide$PBI_phase1_ERROR[s] = pbi_data$PBI_phase1_ERROR[index];
    subLevelWide$PBI_phase2_ERROR[s] = pbi_data$PBI_phase2_ERROR[index];
    subLevelWide$PBI_phase1_RT[s] = pbi_data$PBI_phase1_RT[index];
    subLevelWide$PBI_phase2_RT[s] = pbi_data$PBI_phase2_RT[index];
  }
}

# Put the PBI data into the Long format too
for(n in 1:nrow(pbi_data)){
  for(nphase in 1:2){
    index = which((pbi_data$subID[n] == subLevelLong$subID) & (subLevelLong$phase == nphase));
    if (nphase == 1){
      subLevelLong$PBI_ERROR[index] = pbi_data$PBI_phase1_ERROR[n]
      subLevelLong$PBI_RT[index] = pbi_data$PBI_phase1_RT[n]
    } else{
      subLevelLong$PBI_ERROR[index] = pbi_data$PBI_phase2_ERROR[n]
      subLevelLong$PBI_RT[index] = pbi_data$PBI_phase2_RT[n]
    }
  }
}

# add the PCA SES component one and scaled affective variables to RDM datasets
for(s in 1:nrow(subLevelLong)){
  rdmGainQualtrics$sesPCA[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$sesPCA[s];
  rdmLossQualtrics$sesPCA[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$sesPCA[s];
  rdmBothQualtrics$sesPCA[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$sesPCA[s];

  rdmGainQualtrics$stai_s_score_scaled[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$stai_s_score_scaled[s];
  rdmGainQualtrics$stai_t_score_scaled[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$stai_t_score_scaled[s];
  rdmGainQualtrics$uclal_score_scaled[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$uclal_score_scaled[s];
  rdmGainQualtrics$pss_score_scaled[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$pss_score_scaled[s];
  
  rdmLossQualtrics$stai_s_score_scaled[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$stai_s_score_scaled[s];
  rdmLossQualtrics$stai_t_score_scaled[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$stai_t_score_scaled[s];
  rdmLossQualtrics$uclal_score_scaled[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$uclal_score_scaled[s];
  rdmLossQualtrics$pss_score_scaled[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$pss_score_scaled[s];
  
  rdmBothQualtrics$stai_s_score_scaled[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$stai_s_score_scaled[s];
  rdmBothQualtrics$stai_t_score_scaled[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$stai_t_score_scaled[s];
  rdmBothQualtrics$uclal_score_scaled[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$uclal_score_scaled[s];
  rdmBothQualtrics$pss_score_scaled[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$pss_score_scaled[s];
  
  rdmGainQualtrics$covq_PAB_q1_personalRisk_scaled[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaled[s];
  rdmGainQualtrics$covq_PAB_q1_personalRisk_scaledNoNA[subLevelLong$subID[s]==rdmGainQualtrics$subID & subLevelLong$phase[s] == rdmGainQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaledNoNA[s];
  
  rdmLossQualtrics$covq_PAB_q1_personalRisk_scaled[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaled[s];
  rdmLossQualtrics$covq_PAB_q1_personalRisk_scaledNoNA[subLevelLong$subID[s]==rdmLossQualtrics$subID & subLevelLong$phase[s] == rdmLossQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaledNoNA[s];
  
  rdmBothQualtrics$covq_PAB_q1_personalRisk_scaled[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaled[s];
  rdmBothQualtrics$covq_PAB_q1_personalRisk_scaledNoNA[subLevelLong$subID[s]==rdmBothQualtrics$subID & subLevelLong$phase[s] == rdmBothQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaledNoNA[s];
}


# we are going to exclude some people so lets save the original number of subjects
subNumB4exclusion = unique(rdmGainQualtrics$subID);  # there are the same number of participants in gain and loss datasets
nSubB4exclusion = length(subNumB4exclusion);

## Apply the exclusions to the data set by place NAs in trials for excluded participants
  # RDM exclusion applies to both gain and loss datasets

# Phase 1:
# RDM: 28 participants excluded
subIDrdmPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$rdmPhs1exclude) & excludePhs1$rdmPhs1exclude==1]


# Qualtrics: 3 participants excluded
subIDqualPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$qualPhs1exclude) & excludePhs1$qualPhs1exclude==1]

# Phase 2:
# RDM: 32 participants excluded
subIDrdmPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$rdmPhs2exclude) & excludePhs2$rdmPhs2exclude==1]


# Qualtrics: 1 participant excluded based on age response in phase
subIDqualPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$qualPhs2exclude) & excludePhs2$qualPhs2exclude==1]


# Put NAs in place of excluded data:
# For now, just exclude based on RDM or Qualtrics, don't apply an RDM or Qualtrics exclusion across both.
# This way, we can include as many people as possible within the separate RDM and Qualtrics analyses and when we want to look at RDM + Qualtrics, the NAs will be there for those we need to exclude.

#
# column order is funky. RDM stuff is in columns 1-5, 7-14, 17-18, and day, phase, and subID are dispersed throughout.
rdmColumns = c(1:5,7:14,17:18);


# Qualtrics stuff starts at column 19, or "stai_s_score" until column 71 or "ses_needbasedCollegeAid_recode" and 73 with the sesPCA
qualColumns = c(19:71,73);


# Put NAs in RDM columns for excluded phase 1 and phase 2 participants in both gain and loss datasets
# gain task
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2,rdmColumns] = NA; # phase 2

# loss task
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDrdmPhs1Exclude & rdmLossQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDrdmPhs2Exclude & rdmLossQualtrics$phase==2,rdmColumns] = NA; # phase 2

# BOTH tasks
rdmBothQualtrics[rdmBothQualtrics$subID %in% subIDrdmPhs1Exclude & rdmBothQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmBothQualtrics[rdmBothQualtrics$subID %in% subIDrdmPhs2Exclude & rdmBothQualtrics$phase==2,rdmColumns] = NA; # phase 2

# Put NAs in Qualtrics columns for excluded phase 1 and phase 2 participants in both gain and loss datasets
# gain task
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDqualPhs1Exclude & rdmGainQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDqualPhs2Exclude & rdmGainQualtrics$phase==2,qualColumns] = NA; # phase 2

# loss task
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDqualPhs1Exclude & rdmLossQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDqualPhs2Exclude & rdmLossQualtrics$phase==2,qualColumns] = NA; # phase 2

# BOTH tasks
rdmBothQualtrics[rdmBothQualtrics$subID %in% subIDqualPhs1Exclude & rdmBothQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmBothQualtrics[rdmBothQualtrics$subID %in% subIDqualPhs2Exclude & rdmBothQualtrics$phase==2,qualColumns] = NA; # phase 2


# Note: if participants were excluded in phase 1, they are not automatically excluded in phase 2!

# Store sub IDs of those who are in phase 1 and phase 2 (post-exclusion)
# Note: the sub IDs in phase 1 and phase 2 is identical across gain and loss tasks because the same participants were excluded across both tasks.

# Participant IDs included in phase 1 RDM
Phs1subIDs = excludePhs1$subID[excludePhs1$rdmPhs1exclud==0];
Phs1nSub = length(Phs1subIDs);

# Participant IDs included in phase 2 RDM
Phs2subIDs = excludePhs2$subID[!is.na(excludePhs2$rdmPhs2exclude) & excludePhs2$rdmPhs2exclude==0];
Phs2nSub = length(Phs2subIDs);

# Participant IDs included in both phases
BothPhsSubIDs = Phs2subIDs[Phs2subIDs %in% Phs1subIDs];
BothPhsnSub = length(BothPhsSubIDs)

# Participant IDs included in EITHER phase 1 or phase 2
EitherPhsSubIDs = subNumB4exclusion[subNumB4exclusion %in% Phs1subIDs | subNumB4exclusion %in% Phs2subIDs];
EitherPhsnSub = length(EitherPhsSubIDs);

# Prior to exclusion, there were 544 participants. After exclusion, we have risky decision-making data for 516 participants in phase 1 and 325 participants in phase 2, and 312 participants that are included in both phases.



# Missed trials:
# Where participants did not respond, an NA is in place for choice and outcome. We are not removing these trials but will make a note of the number of trials per phase that were missed by each participants.
# At this point in the script, we have NAs for people who are excluded.

### Which trials were missed?
# GAIN TASK
nanIndGainPhs1 = which(is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$subID %in% Phs1subIDs & rdmGainQualtrics$phase ==1) # 756 missed trials indices phase 1
nanIndGainPhs2 = which(is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$subID %in% Phs2subIDs & rdmGainQualtrics$phase ==2)  # missed trials indices phase 2
nanIndGain = c(nanIndGainPhs1, nanIndGainPhs2);

nanGainPhs1tot = length(nanIndGainPhs1); #756 missed trials phase 1
nanGainPhs2tot = length(nanIndGainPhs2); #372 missed trials phase 2


# LOSS TASK
nanIndLossPhs1 = which(is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$subID %in% Phs1subIDs & rdmLossQualtrics$phase ==1) # missed trials indices phase 1
nanIndLossPhs2 = which(is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$subID %in% Phs2subIDs & rdmLossQualtrics$phase ==2); # missed trials indices phase 2
nanIndLoss = c(nanIndLossPhs1, nanIndLossPhs2);

nanLossPhs1tot = length(nanIndLossPhs1); #245 missed trials phase 1
nanLossPhs2tot = length(nanIndLossPhs2); #88 missed trials phase 2


# NOTE re: "Both": We do not do this for the BOTH tasks 
# dataset b/c it's just the compiled 
# version of Gain & Loss.


### Which participants missed trials and how many did each participant miss?
# GAIN TASK
subNanGainPhs1 = unique(rdmGainQualtrics$subID[nanIndGainPhs1]); # 294 participants missed at least one trial
subNanGainPhs2 = unique(rdmGainQualtrics$subID[nanIndGainPhs2]); # 161 participants missed at least one trial

# LOSS TASK
subNanLossPhs1 = unique(rdmLossQualtrics$subID[nanIndLossPhs1]); # 166 participants missed at least one trial
subNanLossPhs2 = unique(rdmLossQualtrics$subID[nanIndLossPhs2]); # 72 participants missed at least one trial


# Create a dataframe that stores subject IDs, missed gain trials phase 1, missed gain trials phase 2, total gain trials phase 1, total gain trials phase 2, missed loss trials phase 1, missed loss trials phase 2, total loss trials phase 1 and total loss trials phase 2.
subID_missTri_totTri = as.data.frame(matrix(data=NA, nrow = nSubB4exclusion, ncol=9, dimnames = list(c(NULL), c("subID", "missGainTriPhs1", "missGainTriPhs2","totalGainTriPhs1", "totalGainTriPhs2", "missLossTriPhs1", "missLossTriPhs2","totalLossTriPhs1", "totalLossTriPhs2"))));

for (s in 1:nSubB4exclusion){

  subID_missTri_totTri$subID[s] = subNumB4exclusion[s]; # store sub IDs


  # Phase 1:
  if(subID_missTri_totTri$subID[s] %in% subIDrdmPhs1Exclude){ # if participant s was excluded, then put NaN for their rows in phase 1

    subID_missTri_totTri$missGainTriPhs1[s] = NaN; # missed gain trials
    subID_missTri_totTri$totalGainTriPhs1[s] = NaN; # total gain trials
    subID_missTri_totTri$missLossTriPhs1[s] = NaN; # miss loss trials
    subID_missTri_totTri$totalLossTriPhs1[s] = NaN; # total loss trials

    }else{ # otherwise, do the following:

    subID_missTri_totTri$missGainTriPhs1[s] = sum(rdmGainQualtrics$subID[nanIndGainPhs1] == subNumB4exclusion[s]); # missed gain trials
    subID_missTri_totTri$totalGainTriPhs1[s] = sum(!is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$phase==1 & rdmGainQualtrics$subID==subNumB4exclusion[s]); # total gain trials
    subID_missTri_totTri$missLossTriPhs1[s] = sum(rdmLossQualtrics$subID[nanIndLossPhs1] == subNumB4exclusion[s]); # missed loss trials
    subID_missTri_totTri$totalLossTriPhs1[s] = sum(!is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$phase==1 & rdmLossQualtrics$subID==subNumB4exclusion[s]); # total loss trials
  }


  # Phase 2:
 if(subID_missTri_totTri$subID[s] %in%  subIDrdmPhs2Exclude | !subID_missTri_totTri$subID[s] %in% Phs2subIDs){ # if participant s was excluded or they didn't participate in phase 2, then put NaN for their rows in phase 2
   subID_missTri_totTri$missGainTriPhs2[s] = NaN; # missed gain trials
   subID_missTri_totTri$totalGainTriPhs2[s] = NaN; # total gain trials
   subID_missTri_totTri$missLossTriPhs2[s] = NaN; # miss loss trials
   subID_missTri_totTri$totalLossTriPhs2[s] = NaN; # total loss trials

 }else {
   subID_missTri_totTri$missGainTriPhs2[s] = sum(rdmGainQualtrics$subID[nanIndGainPhs2] == subNumB4exclusion[s]);
   subID_missTri_totTri$totalGainTriPhs2[s] = sum(!is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$phase==2 & rdmGainQualtrics$subID==subNumB4exclusion[s])
   subID_missTri_totTri$missLossTriPhs2[s] = sum(rdmLossQualtrics$subID[nanIndLossPhs2] == subNumB4exclusion[s]);
   subID_missTri_totTri$totalLossTriPhs2[s] = sum(!is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$phase==2 & rdmLossQualtrics$subID==subNumB4exclusion[s])
 }

};


# save this dataframe
subID_missTri_totTri_OutputPath = file.path(config$path$combined, config$RDMcsvs$RDM_missed_total_trials)
write.csv(file=subID_missTri_totTri_OutputPath, subID_missTri_totTri, row.names = F);





## CREATING NEW VARIABLES FOR ANALYSES!

# add a new variable for phase where phase 1 is now 0 and phase 2 is now 1
# gain task
rdmGainQualtrics$phaseRecode = rdmGainQualtrics$phase;
rdmGainQualtrics$phaseRecode[rdmGainQualtrics$phaseRecode==1] = 0;
rdmGainQualtrics$phaseRecode[rdmGainQualtrics$phaseRecode==2] = 1;

# loss task
rdmLossQualtrics$phaseRecode = rdmLossQualtrics$phase;
rdmLossQualtrics$phaseRecode[rdmLossQualtrics$phaseRecode==1] = 0;
rdmLossQualtrics$phaseRecode[rdmLossQualtrics$phaseRecode==2] = 1;

# Both tasks
rdmBothQualtrics$phaseRecode = rdmBothQualtrics$phase;
rdmBothQualtrics$phaseRecode[rdmBothQualtrics$phaseRecode==1] = 0;
rdmBothQualtrics$phaseRecode[rdmBothQualtrics$phaseRecode==2] = 1;


## Create a function that creates recent event variables for CAP dataset:

cap_past_event_variable <- function(DFname, DFwithVariable, trialsBack, DFwithSubID, DFwithPhase, scaled){
  # DFname = name of the dataframe
  # DFwithVariable = full name of dataframe + variable name that we want to shift (e.g. rdmGainQualtrics$outcome)
  # trialsBack = numeric; how many trials are we look back?
  # DFwithSubID = full name of dataframe + sub id variable (e.g. rdmGainQualtrics$subID)
  # DFwithPhase = full name of dataframe + phase variable (e.g. rdmGainQualtrics$phase)
  # scaled = 1 = yes, 0 = no (scaled by max risky gain amount)

  newMat = as.data.frame(matrix(data=NA,nrow=nrow(DFname), ncol=4), dimnames=list(c(NULL), c("newVar", "subDiff", "phaseDiff", "taskDiff")));

  newMat$newVar <- DFwithVariable; #take data from columns
  newMat$newVar[(trialsBack + 1):nrow(newMat)] <- newMat$newVar[1:(nrow(newMat)-trialsBack)]; # removes first row, shifts everything up
  newMat$newVar[1:trialsBack] <- NaN #put Nan in for rows that we shifted everything back by

  newMat$subDiff<-c(0,diff(DFwithSubID)); #note when sub ID changes
  newMat$phaseDiff<-c(0,diff(DFwithPhase)); # note when phase changes


  subIDchange = which(newMat$subDiff!=0); # where there is a subject id change
  phasechange = which(newMat$phaseDiff!=0); # where there is a phase change

  newMat$newVar[subIDchange] = NaN
  newMat$newVar[phasechange] = NaN

  if(trialsBack>1){ # if we want to go back more than one trial
    for (t in 1:(trialsBack-1)) {
      newMat$newVar[subIDchange+t] = NaN
      newMat$newVar[phasechange+t] = NaN
    }
  }

  return(newMat$newVar)
}


# Create past outcome variables:
# gain task:
rdmGainQualtrics$rdmPOC1 = cap_past_event_variable(rdmGainQualtrics,rdmGainQualtrics$rdmOutcome, 1, rdmGainQualtrics$subID,rdmGainQualtrics$phase); # outcome t-1
rdmGainQualtrics$rdmPOC2 = cap_past_event_variable(rdmGainQualtrics,rdmGainQualtrics$rdmOutcome, 2, rdmGainQualtrics$subID,rdmGainQualtrics$phase); # outcome t-2

# loss task:
rdmLossQualtrics$rdmPOC1 = cap_past_event_variable(rdmLossQualtrics,rdmLossQualtrics$rdmOutcome, 1, rdmLossQualtrics$subID,rdmLossQualtrics$phase); # outcome t-1
rdmLossQualtrics$rdmPOC2 = cap_past_event_variable(rdmLossQualtrics,rdmLossQualtrics$rdmOutcome, 2, rdmLossQualtrics$subID,rdmLossQualtrics$phase); # outcome t-2

# Both tasks:
rdmBothQualtrics$rdmPOC1 = cap_past_event_variable(rdmBothQualtrics,rdmBothQualtrics$rdmOutcome, 1, rdmBothQualtrics$subID,rdmBothQualtrics$phase); # outcome t-1
rdmBothQualtrics$rdmPOC2 = cap_past_event_variable(rdmBothQualtrics,rdmBothQualtrics$rdmOutcome, 2, rdmBothQualtrics$subID,rdmBothQualtrics$phase); # outcome t-2

# Scale variables:
scaleby = max(rdmGainQualtrics$rdmRiskyGain, na.rm = T);
rdmGainQualtrics$rdmGainSC = rdmGainQualtrics$rdmRiskyGain/scaleby;
rdmGainQualtrics$rdmSafeSC = rdmGainQualtrics$rdmAlternative/scaleby;
rdmGainQualtrics$rdmPOC1sc = rdmGainQualtrics$rdmPOC1/scaleby;
rdmGainQualtrics$rdmPOC2sc = rdmGainQualtrics$rdmPOC2/scaleby;


scaleby = max(rdmBothQualtrics$rdmRiskyGain, na.rm = T);
rdmBothQualtrics$rdmGainSC = rdmBothQualtrics$rdmRiskyGain/scaleby;
rdmBothQualtrics$rdmLossSC = rdmBothQualtrics$rdmRiskyLoss/scaleby;
rdmBothQualtrics$rdmSafeSC = rdmBothQualtrics$rdmAlternative/scaleby;
rdmBothQualtrics$rdmPOC1sc = rdmBothQualtrics$rdmPOC1/scaleby;
rdmBothQualtrics$rdmPOC2sc = rdmBothQualtrics$rdmPOC2/scaleby;

# Calculate cumulative earnings (within each phase) for each participant and scale trial so that it is 0-1 for each participant
# earnings will be 0 to 1, normalized by each participant's max earnings
# save the max cumulative earnings for each person in a vector

# NOTE re: "Both": We do not do this for the Both dataset, as it's not clear
# how to handle earnings across the gain and loss tasks at this time. 

earningsByPhase = vector(); # to store all earnings for each participant
earningsByPhaseScaled = vector(); # to store earnings scaled by each participants' max earnings within each phase
trialByPhase = vector(); # to store scaled trial for each participant

maxEarnSubPhase= as.data.frame(matrix(data=NA, nrow = nSubB4exclusion, ncol = 3, dimnames=list(c(NULL), c("subID","maxEarnPhs1", "maxEarnPhs2"))));
maxEarnSubPhase$subID = 1:nSubB4exclusion;

for (s in 1:nSubB4exclusion) {
  sub = rdmGainQualtrics[rdmGainQualtrics$subID==subNumB4exclusion[s],]
  earningsSub = vector(); # reset earnings vector for each participant
  trialScaled = vector(); # reset trial vector for each participant
  earningsSubScaled = vector(); # reset scaled earnings vector for each participant

  # cumsum function below breaks with NAs, and using na.omit leads cumsum to skip those trials which we don't want. For missed trials ,we want the earnings to be the same as the previous trial, so we deal with it by doing the following:
  tmp = sub$rdmOutcome; # store subjects outcomes
  miss = is.na(tmp); # where are the missing trials?
  tmp[miss] = 0; # replace missing outcomes (NAs) with a 0


  if (length(unique(sub$phase))==1){ # if sub has phase 1 data only:

    earningsSub = cumsum(tmp); # skip over Nas when calculating cumulative earnings
    trialScaled = sub$rdmTrial[sub$phase==1]/max(sub$rdmTrial[sub$phase==1]); # scaled trial by max number of trials for participant in phase 1
    maxEarnSubPhase$maxEarnPhs1[s] = max(earningsSub);
    earningsSubScaled = earningsSub/max(earningsSub);

  } else { # if sub has data from phase 1 and 2:
    earningsSub = c(cumsum(tmp[sub$phase==1]), cumsum(tmp[sub$phase==2]));
    trialScaled = c(sub$rdmTrial[sub$phase==1]/max(sub$rdmTrial[sub$phase==1]),sub$rdmTrial[sub$phase==2]/max(sub$rdmTrial[sub$phase==2]));
    maxEarnSubPhase$maxEarnPhs1[s] = max(cumsum(tmp[sub$phase==1]));
    maxEarnSubPhase$maxEarnPhs2[s] = max(cumsum(tmp[sub$phase==2]));

    earningsSubScaled = c(cumsum(tmp[sub$phase==1])/max(cumsum(tmp[sub$phase==1])), cumsum(tmp[sub$phase==2])/max(cumsum(tmp[sub$phase==2])));
  }

  earningsByPhase = c(earningsByPhase,earningsSub);
  trialByPhase = c(trialByPhase,trialScaled);
  earningsByPhaseScaled = c(earningsByPhaseScaled,earningsSubScaled)

}


rdmGainQualtrics$rdmEarnings = earningsByPhase;
rdmGainQualtrics$rdmEarningSC = earningsByPhaseScaled;
rdmGainQualtrics$rdmTrialSC = trialByPhase;

# Replace 0s in the maxEarnSubPhase dataframe with NAs (there are 0s because we had to make them 0s for cumsum to work the way we want)
maxEarnSubPhase$maxEarnPhs1[maxEarnSubPhase$maxEarnPhs1==0]= NA;
maxEarnSubPhase$maxEarnPhs2[maxEarnSubPhase$maxEarnPhs2==0]= NA;


# for excluded participants, replace 0s in earnings and trial varibles with NA
rdmGainQualtrics$rdmEarnings[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1] = NA;
rdmGainQualtrics$rdmEarnings[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2] = NA;

rdmGainQualtrics$rdmEarningsSC[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1] = NA;
rdmGainQualtrics$rdmEarningsSC[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2] = NA;

rdmGainQualtrics$rdmTrial[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1] = NA;
rdmGainQualtrics$rdmTrial[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2] = NA;

rdmGainQualtrics$rdmTrialSC[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1] = NA;
rdmGainQualtrics$rdmTrialSC[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2] = NA;

# quick summary about cumulative earnings:
# phase 1: range = $1394 - $2549, median = $1891, mean = $1901
# phase 2: range = $1354 - $2700, median = $1870, mean = $1890



# Make a day overall variable (1:40 instead of 1:20) because even though 1:20 is true within-phase, its 1:40 across phases (or across the entire experiment)
rdmGainQualtrics$dayOverall = rdmGainQualtrics$day + rdmGainQualtrics$phaseRecode*20;
rdmLossQualtrics$dayOverall = rdmLossQualtrics$day + rdmLossQualtrics$phaseRecode*20;
rdmBothQualtrics$dayOverall = rdmBothQualtrics$day + rdmBothQualtrics$phaseRecode*20;

# Rescale day to be 0-1
rdmGainQualtrics$daySC = rdmGainQualtrics$day/max(rdmGainQualtrics$day)
rdmGainQualtrics$dayOverallSC = rdmGainQualtrics$dayOverall/max(rdmGainQualtrics$dayOverall)

rdmLossQualtrics$daySC = rdmLossQualtrics$day/max(rdmLossQualtrics$day)
rdmLossQualtrics$dayOverallSC = rdmLossQualtrics$dayOverall/max(rdmLossQualtrics$dayOverall)

rdmBothQualtrics$daySC = rdmBothQualtrics$day/max(rdmBothQualtrics$day)
rdmBothQualtrics$dayOverallSC = rdmBothQualtrics$dayOverall/max(rdmBothQualtrics$dayOverall)

# For gain only task, create variables for shift analysis
rdmGainQualtrics$signedShift = c(0, diff(rdmGainQualtrics$rdmGroundEV));
rdmGainQualtrics$signedShift[rdmGainQualtrics$rdmTrial==1] = 0; # first trial is always 0
rdmGainQualtrics$posShift = rdmGainQualtrics$signedShift*as.numeric(rdmGainQualtrics$signedShift>0);
rdmGainQualtrics$negShift = rdmGainQualtrics$signedShift*as.numeric(rdmGainQualtrics$signedShift<0);

# Edit perceived COVID risk variables
rdmGainQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA = (rdmGainQualtrics$covq_PAB_q1_personalRisk-5)/4;
rdmGainQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA[is.na(rdmGainQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA)] = 0;

rdmBothQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA = (rdmBothQualtrics$covq_PAB_q1_personalRisk-5)/4;
rdmBothQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA[is.na(rdmBothQualtrics$covq_PAB_q1_personalRisk_centeredscaledNoNA)] = 0;


# Add DAY information to subLevelLong. 
subLevelLong$dayOverall = NA; # initialize
for(n in 1:nrow(subLevelLong)){
  # Pull out the unique day corresponding to a given subject & phase
  whichday = unique(rdmGainQualtrics$dayOverall[(rdmGainQualtrics$subID == subLevelLong$subID[n]) & # correct subject
                                                  (rdmGainQualtrics$phase == subLevelLong$phase[n])]) # correct phase
  if(length(whichday) == 0){ 
    next # if they didn't participate (no rdmGainQualtrics entry), then skip it
  } else {
    subLevelLong$dayOverall[n] = whichday;
  }
}



cat('Decision-making data setup complete.\n\n')
