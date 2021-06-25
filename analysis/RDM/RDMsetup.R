# Setting up data for RDManalysis.Rmd which uses 'source' to call this script
# This script has to be an .R not .Rmd to run
# This script loads the risky decision making data and the exclusion data, applies the exclusion, deals with missed trials, and creates any variables we need for the analysis (e.g. past outcome)


# output:
# 1) RDM_subID_missTri_totTri.csv (total and missed trials for each participant in phase for gain and loss datasets)

# Hayley Brooks, University of Denver

# load packages
library('config')
library("lme4");
library("ggplot2");
library("ggExtra");

# configuration
config = config::get()

# load data 
rdmGain_csv = file.path(config$path$combined, config$RDMcsvs$RDMgain_qualtrics); # gain only task path
rdmLoss_csv = file.path(config$path$combined, config$RDMcsvs$RDMloss_qualtrics); # loss only task path
exclsnPhs1_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs1exclusion); # phase 1 exclusion path
exclsnPhs2_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs2exclusion); # phase 2 exclusion path

rdmGainQualtrics = read.csv(rdmGain_csv); # loads gain only RDM + Qualtrics data both phases (takes several seconds)
rdmLossQualtrics = read.csv(rdmLoss_csv); # loads loss only RDM + Qualtrics data both phases (takes several seconds)
excludePhs1 = read.csv(exclsnPhs1_csv); # loads exclusion for phase 1
excludePhs2 = read.csv(exclsnPhs2_csv); # loads exclusion for phase 2

# Remove the extra "X" column present as the first column in datasets
rdmGainQualtrics = rdmGainQualtrics[,(2:ncol(rdmGainQualtrics))];
rdmLossQualtrics = rdmLossQualtrics[,(2:ncol(rdmLossQualtrics))];
excludePhs1 = excludePhs1[,(2:ncol(excludePhs1))];
excludePhs2 = excludePhs2[,(2:ncol(excludePhs2))];

# we are going to exclude some people so lets save the original number of subjects
subNumB4exclusion = unique(rdmGainQualtrics$subID);  # there are the same number of participants in gain and loss datasets
nSubB4exclusion = length(subNumB4exclusion); 


## Apply the exclusions to the data set by place NAs in trials for excluded participants
  # RDM exclusion applies to both gain and loss datasets

# Phase 1: 
# RDM: 26 participants excluded
subIDrdmPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$rdmPhs1exclude) & excludePhs1$rdmPhs1exclude==1]

# Qualtrics: 3 participants excluded
subIDqualPhs1Exclude = excludePhs1$subID[!is.na(excludePhs1$qualPhs1exclude) & excludePhs1$qualPhs1exclude==1]

# Phase 2: 
# RDM: 31 participants excluded
subIDrdmPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$rdmPhs2exclude) & excludePhs2$rdmPhs2exclude==1]

# Qualtrics: 0 participants excluded
subIDqualPhs2Exclude = excludePhs2$subID[!is.na(excludePhs2$qualPhs2exclude) & excludePhs2$qualPhs2exclude==1]


# Put NAs in place of excluded data:
# For now, ust exclude based on RDM or Qualtrics, don't apply an RDM or Qualtrics exclusion across both. 
# This way, we can include as many people as possible within the separate RDM and Qualtrics analyses and when we want to look at RDM + Qualtrics, the NAs will be there for those we need to exclude.

# 
# column order is funky. RDM stuff is in columns 1-5, 7-14, 17-18, and day, phase, and subID are dispersed throughout.
rdmColumns = c(1:5,7:14,17:18);


# Qualtrics stuff starts at column 19, or "stai_s_score" until column 70 or "ses_needbasedCollegeAid_recode"
qualColumns = c(19:70);


# Put NAs in RDM columns for excluded phase 1 and phase 2 participants in both gain and loss datasets
# gain task
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDrdmPhs1Exclude & rdmGainQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDrdmPhs2Exclude & rdmGainQualtrics$phase==2,rdmColumns] = NA; # phase 2

# loss task
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDrdmPhs1Exclude & rdmLossQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDrdmPhs2Exclude & rdmLossQualtrics$phase==2,rdmColumns] = NA; # phase 2

# Put NAs in Qualtrics columns for excluded phase 1 and phase 2 participants in both gain and loss datasets
# gain task
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDqualPhs1Exclude & rdmGainQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmGainQualtrics[rdmGainQualtrics$subID %in% subIDqualPhs2Exclude & rdmGainQualtrics$phase==2,qualColumns] = NA; # phase 2

# loss task
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDqualPhs1Exclude & rdmLossQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmLossQualtrics[rdmLossQualtrics$subID %in% subIDqualPhs2Exclude & rdmLossQualtrics$phase==2,qualColumns] = NA; # phase 2


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

# Prior to exclusion, there were 544 participants. After exclusion, we have risky decision-making data for 518 participants in phase 1 and 326 participants in phase 2, and 313 participants that are included in both phases.



# Missed trials:
# Where participants did not respond, an NA is in place for choice and outcome. We are not removing these trials but will make a note of the number of trials per phase that were missed by each participants.

### Which trials were missed?
# GAIN TASK
nanIndGain = which(is.na(rdmGainQualtrics$rdmChoice)); # both phases
nanIndGainPhs1 = which(is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$phase==1); # phase 1 missed trial
nanIndGainPhs2 = which(is.na(rdmGainQualtrics$rdmChoice) & rdmGainQualtrics$phase==2); # phase 2 missed trials
nanIndGainPhs1tot = length(nanIndGainPhs1); # 3850 missed trials in phase 1
nanIndGainPhs2tot = length(nanIndGainPhs2); # 4062 missed trials in phase 2

# LOSS TASK
nanIndLoss = which(is.na(rdmLossQualtrics$rdmChoice)); # both phases
nanIndLossPhs1 = which(is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$phase==1); # phase 1 missed trial
nanIndLossPhs2 = which(is.na(rdmLossQualtrics$rdmChoice) & rdmLossQualtrics$phase==2); # phase 2 missed trials
nanIndLossPhs1tot = length(nanIndLossPhs1); # 736 missed trials in phase 1
nanIndLossPhs2tot = length(nanIndLossPhs2); # 708 missed trials in phase 2

# Summary:
# There are a total of 7912 missed trials in the gain-only task across all participants and phases (3850 missed trials phase 1 and 4062 missed trials phase 2). There are a total of 1444 missed trials in the loss-only task across all participants and phases (736 missed trials in phase 1 and 708 missed trials in phase 2). Across both tasks and phases, there is a total of 9356 miss trials.

### Which participants missed trials and how many did each participant miss?
# GAIN TASK
subNanGainPhs1 = unique(rdmGainQualtrics$subID[nanIndGainPhs1]); # 320 participants missed at least one trial
subNanGainPhs2 = unique(rdmGainQualtrics$subID[nanIndGainPhs2]); # 193 participants missed at least one trial

# LOSS TASK
subNanLossPhs1 = unique(rdmLossQualtrics$subID[nanIndLossPhs1]); # 192 participants missed at least one trial
subNanLossPhs2 = unique(rdmLossQualtrics$subID[nanIndLossPhs2]); # 103 participants missed at least one trial


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





## Creating variables for analysis - for much of this, we will want to do the same thing for both gain-only and loss-only datasets.

# add a new variable for phase where phase 1 is now 0 and phase 2 is now 1
# gain task
rdmGainQualtrics$phaseRecode = rdmGainQualtrics$phase;
rdmGainQualtrics$phaseRecode[rdmGainQualtrics$phaseRecode==1] = 0;
rdmGainQualtrics$phaseRecode[rdmGainQualtrics$phaseRecode==2] = 1;

# loss task
rdmLossQualtrics$phaseRecode = rdmLossQualtrics$phase;
rdmLossQualtrics$phaseRecode[rdmLossQualtrics$phaseRecode==1] = 0;
rdmLossQualtrics$phaseRecode[rdmLossQualtrics$phaseRecode==2] = 1;



## Create a function that creates recent event variables for CAP dataset:

cap_past_event_variable <- function(DFname, DFwithVariable, trialsBack, DFwithSubID, DFwithPhase){
  
  newMat = as.data.frame(matrix(data=NA,nrow=nrow(DFname), ncol=3), dimnames=list(c(NULL), c("newVar", "subDiff", "phaseDiff", "taskDiff")));
  
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


