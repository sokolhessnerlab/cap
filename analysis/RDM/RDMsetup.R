# Setting up data for RDManalysis.Rmd which uses 'source' to call this script
# This script has to be an .R not .Rmd to run
# This script loads the risky decision making data and the exclusion data, applies the exclusion, deals with missed trials, and creates any variables we need for the analysis (e.g. past outcome)

# load packages
library('config')
library("lme4");
library("ggplot2");
library("ggExtra");

# configuration
config = config::get()

# load data 
rdm_csv = file.path(config$path$combined, config$RDMcsvs$RDM_qualtrics);
exclsnPhs1_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs1exclusion);
exclsnPhs2_csv = file.path(config$path$combined, config$EXCLUSIONcsvs$RDM_AX_Qual_Phs2exclusion);

rdmQualtrics = read.csv(rdm_csv); # loads RDM + Qualtrics data both phases (takes several seconds)
excludePhs1 = read.csv(exclsnPhs1_csv); # loads exclusion for phase 1
excludePhs2 = read.csv(exclsnPhs2_csv); # loads exclusion for phase 2

# Remove the extra "X" column present as the first column in datasets
rdmQualtrics = rdmQualtrics[,(2:ncol(rdmQualtrics))];
excludePhs1 = excludePhs1[,(2:ncol(excludePhs1))];
excludePhs2 = excludePhs2[,(2:ncol(excludePhs2))];

# we are going to exclude some people so lets save the original number of subjects
subNumB4exclusion = unique(rdmQualtrics$subID);
nSubB4exclusion = length(subNumB4exclusion);


## Apply the exclusions to the data set by place NAs in trials for excluded participants

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
#rdmColNames = c("rdmRiskyGain", "rdmRiskyLoss","rdmAlternative", "rdmIsi", "rdmIti","rdmItiExtra","rdmRT" ,"rdmOutcome","rdmChoice","rdmGroundEV","rdmEvInd","rdmRunSize" ,"rdmOutcomeType", "rdmTask","rdmTrial");


# Qualtrics stuff starts at column 19, or "stai_s_score" until column 70 or "ses_needbasedCollegeAid_recode"
qualColumns = c(19:70);

#qualColNames = c("stai_s_score", "stai_t_score","pss_score", "pss_stressedToday","uclal_score", "covq_PAB_q1_personalRisk", "covq_PAB_q2_threat", "covq_PAB_q3_personallyDie","covq_PAB_q4_otherPersonDie", "covq_attentionCheck_select_6","covq_attentionCheck_passed", "covq_PAB_q5_currentCases", "covq_PAB_q5_currentCases_recode", "covq_PAB_q6_tested","covq_PAB_q6_tested_recode", "covq_PAB_q7_personalCovidSuspect", "covq_PAB_q7_personalCovidSuspect_recode", "covq_PAB_q8_knowPositivePerson","covq_PAB_q8_knowPositivePerson_recode", "covq_PAB_q9_socialDistanceLevel", "covq_PAB_q9_socialDistanceLevel_recode", "demo_gender","demo_gender_recode", "demo_race","demo_race_recode", "demo_ethnicity","demo_ethnicity_recode", "loc_state","loc_county", "loc_fips", "ses_childhood_freeReducedLunch", "ses_childhood_freeReducedLunch_recode","ses_childhood_communityComp", "ses_childhood_communityComp_recode","ses_childhood_nationalComp", "ses_childhood_nationalComp_recode", "ses_motherEdLevel", "ses_motherEdLevel_recode","ses_fatherEdLevel" , "ses_fatherEdLevel_recode", "ses_childhoood_homeOwnership", "ses_childhoood_homeOwnership_recode","ses_current_billHelp", "ses_current_billHelp_recode","ses_current_mainResponsibilities", "ses_current_mainResponsibilities_recode", "ses_personalEdLevel", "ses_personalEdLevel_recode" ,"ses_financialWorryFreq","ses_financialWorryFreq_recode", "ses_needbasedCollegeAid", "ses_needbasedCollegeAid_recode")


# Put NAs in RDM columns for excluded phase 1 and phase 2 participants
rdmQualtrics[rdmQualtrics$subID %in% subIDrdmPhs1Exclude & rdmQualtrics$phase==1,rdmColumns] = NA; # phase 1
rdmQualtrics[rdmQualtrics$subID %in% subIDrdmPhs2Exclude & rdmQualtrics$phase==2,rdmColumns] = NA; # phase 2


# Put NAs in Qualtrics columns for excluded phase 1 and phase 2 participants
rdmQualtrics[rdmQualtrics$subID %in% subIDqualPhs1Exclude & rdmQualtrics$phase==1,qualColumns] = NA; # phase 1
rdmQualtrics[rdmQualtrics$subID %in% subIDqualPhs2Exclude & rdmQualtrics$phase==2,qualColumns] = NA; # phase 2

# Note: if participants were excluded in phase 1, they are not automatically excluded in phase 2!


#   THIS PART IS NOT COMPLETE - CONTINUE ONCE WE HAVE SPEARATED LOSS DATASET
# When participants do not respond, an NA is place for choice and outcome. We are not removing these trials but will make a note of the number of trials per phase were missed by which participants.
# 
# nanInd = which(is.na(rdmQualtrics$rdmChoice)); 
# nanIndPhs1 = which(is.na(rdmQualtrics$rdmChoice) & rdmQualtrics$phase==1);
# nanIndPhs2 = which(is.na(rdmQualtrics$rdmChoice) & rdmQualtrics$phase==2);
# totNan = length(nanInd); # 9356 missed trials across all participants and phases
# totNanPhs1 = length(nanIndPhs1); # 4586 missed trials phase 1
# totNanPhs2 = length(nanIndPhs2); # 4770 missed trials phase 2
# 
# subNanPhs1 = unique(rdmQualtrics$subID[nanIndPhs1]); # sub IDs of those who missed trials in phase 1
# subNanPhs2 = unique(rdmQualtrics$subID[nanIndPhs2]); # sub IDs of those who missed trials in phase 2
#   
# subID_missTri_totTri = as.data.frame(matrix(data=NA, nrow = nSubB4exclusion, ncol=5, dimnames = list(c(NULL), c("subID", "missTriPhs1", "missTriPhs2","totalTriPhs1", "totalTriPhs2"))));
# 
# for (s in 1:nSubB4exclusion){
#   subID_missTri_totTri$subID[s] = subNumB4exclusion[s]
#   subID_missTri_totTri$missTriPhs1[s] = sum(rdmQualtrics$subID[nanIndPhs1] == subNumB4exclusion[s]);
#   subID_missTri_totTri$missTriPhs2[s] = sum(rdmQualtrics$subID[nanIndPhs2] == subNumB4exclusion[s]);
#   
#   
# };
# 
# # Remove missed trials
# #rdmQualtrics = rdmQualtrics[which(!is.nan(mriBehClean$choice)),]; #remove missed trials
# 
# #how many trials does each person have now that we have cleaned the data?
# for (s in 1:nSubB4exclusion){
#   
#   subID_missTri_totTri$totalTriPhs1[s] = sum(rdmQualtrics$subID %in% subNumB4exclusion[s] & rdmQualtrics$phase==1);
#   subID_missTri_totTri$totalTriPhs2[s] = sum(rdmQualtrics$subID %in% subNumB4exclusion[s] & rdmQualtrics$phase==2);
# };
# 
# 
# # save this matrix
# save(file="/Volumes/shlab/Projects/VNI/data/mriBehaviorQAoutput/subID_missedT_totalT_pGam_N48.Rdata", subID_missTri_totTri_pGam);








## Creating variables for analysis - was in the process of making a function to do the recent event variables and then we decided to separate loss task from RDM gain only - come back to this once that separation is taken care of.
# add a new variable for phase where phase 1 is now 0 and phase 2 is now 1
rdmQualtrics$phaseRecode = rdmQualtrics$phase;
rdmQualtrics$phaseRecode[rdmQualtrics$phaseRecode==1] = 0;
rdmQualtrics$phaseRecode[rdmQualtrics$phaseRecode==2] = 1;

#   still working on - dealing with NAN when trials back is more than 2
# putting in a nan for the loss trials

# Function for create recent event variables for CAP dataset
cap_past_event_variable <- function(DFname, DFwithVariable, trialsBack, DFwithSubID, DFwithPhase, DFwithTask, newVariable){
  
  newMat = as.data.frame(matrix(data=NA,nrow=nrow(DFname), ncol=3), dimnames=list(c(NULL), c("newVar", "subDiff", "phaseDiff", "taskDiff")));
  
  newMat[,1] <- DFwithVariable; #take data from columns
  newMat[(trialsBack + 1):nrow(newMat),1] <- newMat[1:(nrow(newMat)-trialsBack),1]; # removes first row, shifts everything up
  newMat[1:trialsBack,1] <- NaN #put Nan in for rows that we shifted everything back by
  
  newMat[,2]<-c(0,diff(DFwithSubID)); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
  newMat[,3]<-c(0,diff(DFwithPhase));
  
  
  subIDchange = which(newMat[,2]!=0,1); # where there is a subject id change
  phasechange = which(newMat[,3]!=0,1); # where there is a phase change
  
  newMat[subIDchange,1] = NaN
  newMat[phasechange,1] = NaN
  
}

DFname = rdmQualtrics
DFwithVariable = rdmQualtrics$rdmOutcome 
trialsBack = 1
DFwithSubID = rdmQualtrics$subID
DFwithPhase = rdmQualtrics$phase
DFwithTask = rdmQualtrics$rdmTask
newVariable = rdmQualtrics$rdmPOC


# non function version:
newMat = as.data.frame(matrix(data=NA,nrow=nrow(rdmQualtrics), ncol=4, dimnames=list(c(NULL), c("newVar", "subDiff", "phaseDiff", "taskDiff"))));
newMat$newVar <- rdmQualtrics$rdmOutcome; #take data from columns
newMat$newVar[2:nrow(newMat)] <- newMat$newVar[1:(nrow(newMat)-1)]; # removes first row, shifts everything up
newMat$newVar[1] <- NaN #put Nan in for first row (first trial for subject 1, bc there is no past trial)

newMat$subDiff<-c(0,diff(rdmQualtrics$subID)); #put differences between NewSubjectIndex into newvector, 1s show up when subject changes
newMat$phaseDiff<-c(0,diff(rdmQualtrics$phase));
newMat$taskDiff<-c(0,diff(rdmQualtrics$rdmTask));

subIDchange = which(newMat$subDiff!=0); # where there is a subject id change
phasechange = which(newMat$phaseDiff!=0); # where there is a phase change
taskchange = which(newMat$taskDiff!=0); # where the task changed (gain only to loss only)


newMat$newVar[subIDchange] = NaN;
newMat$newVar[phasechange] = NaN;
newMat$newVar[taskchange] = NaN;


# how to fill in NaN when trials back is more than 1 trial:
#if(trialsBack >1){

#}



rdmQualtrics$rdmPOC = newMat$newVar;# add new vector to
