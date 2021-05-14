# Exclusion for CAP study 
# Identifying those who may need to be excluded from risky decision-making task
# OUTPUT: two .csv files (CAP/data/combinedData/)
  # 1) RDMqa.csv
  # 2) rdmExclusion.csv

# There are four criteria to consider for exclusion based on RDM data
# 1) missed trials
# 2) missed attention checks (e.g. gambled when safe was at least the same as risky gain or played it safe when alt was $0)
# 3) p(gamble) close to the bounds
# 4) choices not influenced by gain and safe amounts on each trial (individual-level glm)

# created 11/18/20 Hayley Brooks

# reset environment
rm(list=ls()); 

# load packagest
library("lme4"); 

# load rdm data (clean, combined data across phases with sub IDs)
rdm = read.csv("/Volumes/CAP/data/combinedData/RDMallClean.csv"); # takes ~5 seconds to load

subNums = unique(rdm$subID); # unique sub ID numbers
nSub = length(subNums); # number of participants


# create data frame to save our quality check results:
RDMqualityCheck = as.data.frame(matrix(data=NA, nrow=nSub, ncol=25, dimnames = list(c(NULL), c("subID", "phase1GainMissed", "phase2GainMissed", "phase1LossMissed","phase2LossMissed", "ShouldSafePhase1tot", "ShouldSafePhase1totMiss", "ShouldSafePhase1MissProportion", "ShouldGamPhase1tot","ShouldGamPhase1totMiss","ShouldGamPhase1MissProportion","ShouldSafePhase2tot","ShouldSafePhase2totMiss","ShouldSafePhase2MissProportion","ShouldGamPhase2tot", "ShouldGamPhase2totMiss", "ShouldGamPhase2MissProportion", "pgamblePhase1gain", "pgamblePhase2gain", "pgamblePhase1loss", "pgamblePhase2loss", "glmGainYesPhase1", "glmSafeYesPhase1", "glmGainYesPhase2", "glmSafeYesPhase2"))));

RDMqualityCheck$subID = subNums; # fill in sub IDs


# 1) MISSED TRIALS (who missed trials and how many did they miss?)
missedTrialsInd = which(is.na(rdm$rdmChoice)); # indices of all missed trials in rdm data
rdmMissedTrials = rdm[missedTrialsInd,]; # dataframe that stores information from 1876 total missed trials

rdmMissedTrialsGain = rdmMissedTrials[rdmMissedTrials$rdmTask==1,];  # 1495 missed trials in gain only task
rdmMissedTrialsLoss = rdmMissedTrials[rdmMissedTrials$rdmTask==2,];  # 381 missed trials in loss task

rdmMissedTrialsGainPhase1 = rdmMissedTrialsGain[rdmMissedTrialsGain$phase==1,]; # 921 missed trials in gain only task phase 1
rdmMissedTrialsGainPhase2 = rdmMissedTrialsGain[rdmMissedTrialsGain$phase==2,]; # 574 missed trials in gain only task phase 2

rdmMissedTrialsLossPhase1 = rdmMissedTrialsLoss[rdmMissedTrialsLoss$phase==1,]; # 271 missed trials in loss task phase 1
rdmMissedTrialsLossPhase2 = rdmMissedTrialsLoss[rdmMissedTrialsLoss$phase==2,]; # 110 missed trials in loss task phase 2

# Summary of missed trials:
#               phase1Gain  phase2Gain  phase1Loss  phase2Loss 
# total trials         921         574         271         110 
# mean trials    1.6930147   1.0551471   0.4981618   0.2022059 
# range               0-36        0-49         0-7         0-5
# range %            0-30%     0-40.1%       0-35%       0-25%


# Store the proportion of missed trials for gain and loss task 
# most participants should have 119 gain only and 20 mixed-valence trials
for (s in 1:nSub) {
  RDMqualityCheck$phase1GainMissed[s] = sum(rdmMissedTrialsGainPhase1$subID==s)/length(rdm$rdmTrial[rdm$subID==s & rdm$rdmTask==1 & rdm$phase==1]); # proportion of miss trials in gain only task in phase 1
  RDMqualityCheck$phase2GainMissed[s] = sum(rdmMissedTrialsGainPhase2$subID==s)/length(rdm$rdmTrial[rdm$subID==s & rdm$rdmTask==1 & rdm$phase==2]); # proportion of miss trials in gain only task in phase 2
  RDMqualityCheck$phase1LossMissed[s] = sum(rdmMissedTrialsLossPhase1$subID==s)/length(rdm$rdmTrial[rdm$subID==s & rdm$rdmTask==2 & rdm$phase==1]); # proportion of miss trials in loss task in phase 1
  RDMqualityCheck$phase2LossMissed[s] = sum(rdmMissedTrialsLossPhase2$subID==s)/length(rdm$rdmTrial[rdm$subID==s & rdm$rdmTask==2 & rdm$phase==2]); # proportion of miss trials in loss task in phase 2
};



# 2) MISSED ATTENTION CHECKS (gain-only task)
# choices where participants gambled and should have played it safe (when safe amount is at least equal to risk gain amount)
# choices where participants did not gamble and should have (when safe is $0)
# Because of the study design, the full VNI choice set was generated (219 trials), then we only used the first 119. The result is that not everyone had the same number of attention checks. When deciding on who "passes" or "fails" this criteria, we will want to consider both the number of attention checks they have and the proportion missed (missing 1 out of 2 is a lot different than missing 6 out of 12).

# Choices where participants should have played it safe
shouldNotGamble = rdm[rdm$rdmTask==1 & rdm$rdmAlternative >= rdm$rdmRiskyGain,]; # 6992 types of these choices across phase 1 and 2
shouldNotGamblePhase1 = shouldNotGamble[shouldNotGamble$phase==1,]; # 4200 types of these choices across phase 1
shouldNotGamblePhase2 = shouldNotGamble[shouldNotGamble$phase==2,]; # 2792 types of these choices across phase 2

# How many of these "should not gamble" choices did participants miss?
shouldNotGambleMiss = shouldNotGamble[shouldNotGamble$rdmChoice==1 | is.na(shouldNotGamble$rdmChoice),]; #256 missed trials (3.6% of these choices were missed)
shouldNotGambleMissPhase1 = shouldNotGambleMiss[shouldNotGambleMiss$phase==1,]; # 158 trials in phase 1 (3.7% of these choices missed)
shouldNotGambleMissPhase2 = shouldNotGambleMiss[shouldNotGambleMiss$phase==2,]; # 98 trials in phase 2 (3.5% of these choices missed)


# Choices where participants should have gambled (safe = $0)
shouldGamble = rdm[rdm$rdmTask==1 & rdm$rdmAlternative==0,]; # 1207 types of these choices across phase 1 and 2
shouldGamblePhase1 = shouldGamble[shouldGamble$phase==1,]; # 755 types of these choices across phase 1
shouldGamblePhase2 = shouldGamble[shouldGamble$phase==2,]; # 452 types of these choices across phase 2

# How many of these "should gamble" choices did participants miss?
shouldGambleMiss = shouldGamble[shouldGamble$rdmChoice==0 | is.na(shouldGamble$rdmChoice),]; #28 trials (2.3% of these choices were missed) 
shouldGambleMissPhase1 = shouldGambleMiss[shouldGambleMiss$phase==1,]; # 18 trials in phase 1 (2.3% of these choices missed)
shouldGambleMissPhase2 = shouldGambleMiss[shouldGambleMiss$phase==2,]; # 10 trials in phase 2 (2.2% of these choices missed)


# Store the total number of attention checks, the number missed, and the proportion of missed attention checks. 
for (s in 1:nSub) {
  
  # how many total "should safe" (i.e. "should not gamble") trials did each participant encounter?
  RDMqualityCheck$ShouldSafePhase1tot[s] = nrow(shouldNotGamblePhase1[shouldNotGamblePhase1$subID==s,]); # phase 1
  RDMqualityCheck$ShouldSafePhase2tot[s] = nrow(shouldNotGamblePhase2[shouldNotGamblePhase2$subID==s,]); # phase 2
  
  # how many of these "should safe" trials did they miss?
  RDMqualityCheck$ShouldSafePhase1totMiss[s] = nrow(shouldNotGambleMissPhase1[shouldNotGambleMissPhase1$subID==s,]); # phase 1
  RDMqualityCheck$ShouldSafePhase2totMiss[s] = nrow(shouldNotGambleMissPhase2[shouldNotGambleMissPhase2$subID==s,]); # phase 2
  
  
  # how many total "should gamble" trials did each participant encounter?
  RDMqualityCheck$ShouldGamPhase1tot[s]=nrow(shouldGamblePhase1[shouldGamblePhase1$subID==s,]); # phase 1
  RDMqualityCheck$ShouldGamPhase2tot[s]=nrow(shouldGamblePhase2[shouldGamblePhase2$subID==s,]); # phase 2
  
  # how many of these "should gamble" trials did they miss?
  RDMqualityCheck$ShouldGamPhase1totMiss[s]=nrow(shouldGambleMissPhase1[shouldGambleMissPhase1$subID==s,]); # phase 1
  RDMqualityCheck$ShouldGamPhase2totMiss[s]=nrow(shouldGambleMissPhase2[shouldGambleMissPhase2$subID==s,]); # phase 2

  
  # What proportion of the "should safe" and "should gamble" trials did participants miss?
    # for participants who did not participate in phase 2, this will be 0/0 = NaN
    # for participants who did not have one of these trials types (choiceset was cut short), this will also be NaN 
    # 0 = no misses; 1 = all missed
  
  # "should safe"
  RDMqualityCheck$ShouldSafePhase1MissProportion[s] = RDMqualityCheck$ShouldSafePhase1totMiss[s]/RDMqualityCheck$ShouldSafePhase1tot[s]; # phase 1
  RDMqualityCheck$ShouldSafePhase2MissProportion[s] = RDMqualityCheck$ShouldSafePhase2totMiss[s]/RDMqualityCheck$ShouldSafePhase2tot[s]; # phase 2
  
  # "should gamble"
  RDMqualityCheck$ShouldGamPhase1MissProportion[s] = RDMqualityCheck$ShouldGamPhase1totMiss[s]/RDMqualityCheck$ShouldGamPhase1tot[s]; # phase 1
  RDMqualityCheck$ShouldGamPhase2MissProportion[s] = RDMqualityCheck$ShouldGamPhase2totMiss[s]/RDMqualityCheck$ShouldGamPhase2tot[s]; # phase 2
  
};


# 3) PROBABILITY OF GAMBLING

for (s in 1:nSub) {
  RDMqualityCheck$pgamblePhase1gain[s] = mean(rdm$rdmChoice[rdm$rdmTask==1 & rdm$phase==1 & rdm$subID==s], na.rm=T); # phase 1, gain-only
  RDMqualityCheck$pgamblePhase2gain[s] = mean(rdm$rdmChoice[rdm$rdmTask==1 & rdm$phase==2 & rdm$subID==s], na.rm=T); # phase 2, gain-only
  
  RDMqualityCheck$pgamblePhase1loss[s] = mean(rdm$rdmChoice[rdm$rdmTask==2 & rdm$phase==1 & rdm$subID==s], na.rm=T); # phase 1, loss
  RDMqualityCheck$pgamblePhase2loss[s] = mean(rdm$rdmChoice[rdm$rdmTask==2 & rdm$phase==2 & rdm$subID==s], na.rm=T); # phase 2, loss
};

# summary of pgamble
# gain-only phase 1: mean = .38; range = 0-.91; median = .39
      # 3 subs pgamble = 0; 8 subs pgamble <.01; 14 subs pgamble <.02; 33 subs pgamble <.05; 62 subs pgamble <.1
      # 1 sub pgamble > .9; 9 subs pgamble > .8
# gain-only phase 2: mean = .35; range = 0-.91; median = .36
      # 7 subs pgamble = 0; 17 subs pgamble <.01; 30 subs pgamble <.02; 45 subs pgamble <.05; 70 subs pgamble <.1
      # 1 sub pgamble > .9; 6 subs pgamble > .8
cor.test(RDMqualityCheck$pgamblePhase1gain, RDMqualityCheck$pgamblePhase2gain, method = "spearman"); # correlation of pgamble across phase 1 and 2
# rho = .64; p <2.2e-16

# loss-only phase 1: mean = .45; range =0-1; median = .44
     # 20 subs pgamble = 0; 36 subs pgmable <.1
     # 22 subs pgmable = 1; 33 subs pgamble >.9
# loss-only phase 2: mean = .43; range =0-1; median = .4
     # 15 subs pgamble = 0; 27 subs pgamble <.1
     # 19 subs pgamble = 1; 24 subs pgamble >.9
cor.test(RDMqualityCheck$pgamblePhase1loss, RDMqualityCheck$pgamblePhase2loss, method = "spearman"); # correlation of pgamble across phase 1 and 2
# rho = .59; p <2.2e-16


# 4) CHOICES INFLUENCED BY GAINS AND SAFE (gain-only task)
# Use glm to run individual-level models to see if the gain and safe values influenced risk-taking
  # glmGainYes: 1 = gain did influence sub's decisions; 0 = gain did not influence sub's decisions
  # glmSafeYes: 1 = safe did influence sub's decisions; 0 = safe did not influence sub's decisions

for (s in 1:nSub) {
  sub = rdm[rdm$subID==s,]; # pull out sub's data
  
  # phase 1
  glmPhs1 = glm(rdmChoice~ rdmRiskyGain + rdmAlternative, data=sub[sub$phase==1,]); # do the glm with phase 1 data
  glmPhs1Summary = summary(glmPhs1); # save the results
  
  if(is.finite(glmPhs1Summary$coefficients[11]) & glmPhs1Summary$coefficients[11] < .05){ # check p-value for risky gain
    RDMqualityCheck$glmGainYesPhase1[s] = 1; # if it is significant, this sub's choices were influenced by gain values
  } else{
    RDMqualityCheck$glmGainYesPhase1[s] = 0; # otherwise, this sub's choices were not influenced by gain values
  };
  
  if(is.finite(glmPhs1Summary$coefficients[12]) & glmPhs1Summary$coefficients[12] < .05){ # check p-value for safe
    RDMqualityCheck$glmSafeYesPhase1[s] = 1; # if it is significant, this sub's choices were influenced by safe values
  } else{
    RDMqualityCheck$glmSafeYesPhase1[s] = 0; # otherwise, this sub's choices were not influenced by safe values
  };
  
  
  # phase 2
  if(any(sub$phase==2)){  # if participant completed phase 2
    glmPhs2 = glm(rdmChoice~ rdmRiskyGain + rdmAlternative, data=sub[sub$phase==2,]); # do the glm with phase 2 data
    glmPhs2Summary = summary(glmPhs2); # save the results
    
    if(is.finite(glmPhs2Summary$coefficients[11]) & glmPhs2Summary$coefficients[11] < .05){ # check p-value for risky gain
      RDMqualityCheck$glmGainYesPhase2[s] = 1; # if it is significant, this sub's choices were influenced by gain values
    } else{
      RDMqualityCheck$glmGainYesPhase2[s] = 0; # otherwise, this sub's choices were not influenced by gain values
    };
    
    if(is.finite(glmPhs2Summary$coefficients[12]) & glmPhs2Summary$coefficients[12] < .05){ # check the p-value for safe
      RDMqualityCheck$glmSafeYesPhase2[s] = 1; # if it is significant, the sub's choices were influenced by safe values
    } else{
      RDMqualityCheck$glmSafeYesPhase2[s] = 0; # otherwise, this sub's choices were not influenced by safe values
    };
    
    
  }; # end if(any(sub$phase==2)) statement

}; # end 1:nSub loop


# save our big table!
write.csv(RDMqualityCheck, "/Volumes/CAP/data/combinedData/RDMqa.csv", row.names = F);



# FLAG PROBLEMATIC PARTICIPANTS who:
  # 1) missed more than 10% of gain-only and mixed-valence trials
  # 2) missed too many attention checks (at least 30% of attention checks and had at least 5 attentio check trials)
  # 3) pgamble is less than .05 and greater than .95 (will also try .03 and .97)
  # 4) were not influenced by gain and safe

#  Treat each person and phase as a unique person (ie. if someone does poorly in phase 1, we won't automatically exclude from phase 2)
# Output 1 & 2 (not saved as file): two dataframes (phase 1 and phase 2) with 6 columns: 
    # sub ID, missed trials gain (1/0), missed trials loss(1/0), missed Attn checks (1/0), pgamble (1/0), glm (1/0), where 1= passed, 0 = failed
# Output 3 (saved as .csv file): Matrix with 3 columns: sub ID, exclude phase 1 (1/0), exclude phase 2 (1/0), where 1 = yes exclude, 0 = no

# Set tresholds:
missTriThresh = .1;     # cut off for proportion of missed trials (i.e. participants "fail" if they missed more than this number)
pgamLowThresh = .05;    # lower end of pgamble threshold (pgambles lower than this will be considered a "fail")
pgamHighThresh = .95;   # higher end of pgamble threshold (pgambles higher than this will be considered a "fail")
missACprop = .3;        # proportion of missed attention checks; (any number higher than this would be a "fail")
minACtotal = 4;         # participant had atleast this number of attention check trials

# Create the data frames for output 1 and 2:
passFailCategories = c("subID","missTriGain","missTriLoss", "missACs", "pgamble", "glm"); # column names
phase1passfail = as.data.frame(matrix(data=NA, nrow=nSub, ncol=length(passFailCategories),dimnames = list(c(NULL), passFailCategories))); # phase1
phase2passfail = as.data.frame(matrix(data=NA, nrow=nSub, ncol=length(passFailCategories),dimnames = list(c(NULL), passFailCategories))); # phase2

# fill in sub IDs
phase1passfail$subID=1:nSub; # phase 1
phase2passfail$subID=1:nSub; # phase 2


# 1)MISSED TRIALS 

# GAIN-ONLY
# phase 1 mean = 1.7, sd=3.2 
# phase 2 mean = 1.1, sd = 3.5 

# phase 1: gain-only
phase1passfail$missTriGain[which(RDMqualityCheck$phase1GainMissed > missTriThresh)] = 0;  # 0 if they missed more trials than threshold
phase1passfail$missTriGain[which(RDMqualityCheck$phase1GainMissed <= missTriThresh)] = 1; # 1 if they missed less trials than threshold

# phase 2: gain-only
phase2passfail$missTriGain[which(RDMqualityCheck$phase2GainMissed > missTriThresh)] = 0;  # 0 if they missed more trials than threshold
phase2passfail$missTriGain[which(RDMqualityCheck$phase2GainMissed <= missTriThresh)] = 1; # 1 if they missed less trials than threshold


# LOSS (MIXED VALENCE TASK)
# phase 1 mean = .49, sd = .94 
# phase 2 mean = .2, sd = .55 

# phase 1: loss
phase1passfail$missTriLoss[which(RDMqualityCheck$phase1LossMissed > missTriThresh)] = 0;  # 0 if they missed more trials than threshold
phase1passfail$missTriLoss[which(RDMqualityCheck$phase1LossMissed <= missTriThresh)] = 1; # 1 if they missed more trials than threshold

# phase 2: loss
phase2passfail$missTriLoss[which(RDMqualityCheck$phase2LossMissed > missTriThresh)] = 0;  # 0 if they missed more trials than threshold
phase2passfail$missTriLoss[which(RDMqualityCheck$phase2LossMissed <= missTriThresh)] = 1; # 1 if they missed more trials than threshold


# 2) ATTENTION CHECKS:

# Phase 1: 
# range of total attention checks per person = 2-15 trials
# range of proportion missed = 0-.8; median = 0; mean = .04

# Which participants had at least the number of attention check trials we set as the threshold and also missed a proportion of these trials above our set threshold?
missACindPhs1 = which((RDMqualityCheck$ShouldGamPhase1totMiss + RDMqualityCheck$ShouldSafePhase1totMiss)/(RDMqualityCheck$ShouldGamPhase1tot + RDMqualityCheck$ShouldSafePhase1tot) > missACprop & (RDMqualityCheck$ShouldGamPhase1tot + RDMqualityCheck$ShouldSafePhase1tot)>minACtotal); 

# check total attention check trials for these participants? Does it make sense to exclude them?
#RDMqualityCheck[missACindPhs1, c("ShouldSafePhase1tot","ShouldSafePhase1totMiss","ShouldGamPhase1tot","ShouldGamPhase1totMiss")];

phase1passfail$missACs[missACindPhs1] = 0;  # 0 = fail
phase1passfail$missACs[-missACindPhs1] = 1; # 1 = pass


# Phase 2:
# range of total attention checks per person = 0-16 trials
# range of proportion missed = 0-.67; median = 0; mean = .04;

# Which participants had at least the number of attention check trials we set as the threshold and also missed a proportion of these trials above our set threshold?
missACindPhs2 = which((RDMqualityCheck$ShouldGamPhase2totMiss + RDMqualityCheck$ShouldSafePhase2totMiss)/(RDMqualityCheck$ShouldGamPhase2tot + RDMqualityCheck$ShouldSafePhase2tot) > missACprop & (RDMqualityCheck$ShouldGamPhase2tot + RDMqualityCheck$ShouldSafePhase2tot)>minACtotal);

#RDMqualityCheck[missACindPhs2, c("ShouldSafePhase2tot","ShouldSafePhase2totMiss","ShouldGamPhase2tot","ShouldGamPhase2totMiss")];

phase2passfail$missACs[missACindPhs2] = 0;  # 0 = fail
phase2passfail$missACs[-missACindPhs2] = 1; # 1 = pass



# 3) PROBABILITY OF GAMBLING NEAR BOUNDS (gain-only)
    # 0 = pgamble higher or lower than threshold
    # 1 = pgamble within and including the threshold limits

# phase 1
phase1passfail$pgamble[which(RDMqualityCheck$pgamblePhase1gain > pgamHighThresh | RDMqualityCheck$pgamblePhase1gain < pgamLowThresh)]=0;
phase1passfail$pgamble[which(RDMqualityCheck$pgamblePhase1gain <= pgamHighThresh & RDMqualityCheck$pgamblePhase1gain >= pgamLowThresh)]=1;

# phase 2
phase2passfail$pgamble[which(RDMqualityCheck$pgamblePhase2gain > pgamHighThresh | RDMqualityCheck$pgamblePhase2gain < pgamLowThresh)]=0;
phase2passfail$pgamble[which(RDMqualityCheck$pgamblePhase2gain <= pgamHighThresh & RDMqualityCheck$pgamblePhase2gain >= pgamLowThresh)]=1;



# 4)  CHOICES INFLUENCED BY GAINS AND SAFE (gain-only task)
  # 0 = not influened by safe or gain values
  # 1 = influenced by safe and gain values

# phase 1
phase1passfail$glm[which(RDMqualityCheck$glmGainYesPhase1==0 | RDMqualityCheck$glmSafeYesPhase1==0)] = 0;  # fail
phase1passfail$glm[which(RDMqualityCheck$glmGainYesPhase1==1 & RDMqualityCheck$glmSafeYesPhase1==1)] = 1;  # pass

# phase 2
phase2passfail$glm[which(RDMqualityCheck$glmGainYesPhase2==0 | RDMqualityCheck$glmSafeYesPhase2==0)] = 0;  # fail
phase2passfail$glm[which(RDMqualityCheck$glmGainYesPhase2==1 & RDMqualityCheck$glmSafeYesPhase2==1)] = 1;  # pass





# BASED ON THE PASS/FAIL IN CRITERIA ABOVE, WHO ARE WE EXCLUDING?
# For each participant, what is the sum of their pass/fail results (5=passed all criteria; 0= passed none)

# What is our cut off for determining who gets excluded and who stays?
    # If our cut off is missing more than 1 (ie we only keep 4s and 5s), we exclude 26 from phase 1 and 31 from phase 2 (10 subs from both phase 1 and 2)
     # This criteria captures a lot (not all) of the participants with glm = 0 (choices not influenced by gain or safe values). 
     # there will be participants w/ 4s who missed the glm criteria 
     #    if data is good otherwise, perhaps their choices were influenced by some of the things we are interested in (e.g. context!)

   # If our cut off is missing more than 2 (we keep 3-5s), we exclude less: 3 from phase 1 and 2 from phase 2
      # this is a more conservative approach but we'd be including people with glm=0 and pgamble near bounds and missed ACs


# How did each participant do?
  # For each participant, store the total of pass/fails, where 5 = perfect, 0 = failed each criteria
phase1passfail$total=rowSums(phase1passfail[2:6]); # range = 2-5; mean = 4.76
phase2passfail$total=rowSums(phase2passfail[2:6]); # range = 1-5; mean = 4.7

# Create data frame for output #3:
rdmExclude = as.data.frame(matrix(data=NA, nrow = nSub, ncol=3, dimnames=list(c(NULL), c("subID", "phase1Exclude", "phase2Exclude"))));

rdmExclude$subID = 1:nSub; # fill in sub ID

excludeCutOff = 4; # we are only keeping those with totals at or above this number

# phase 1:
rdmExclude$phase1Exclude[phase1passfail$total<excludeCutOff] = 1; # exclude if they missed more than 2 
rdmExclude$phase1Exclude[phase1passfail$total>=excludeCutOff] = 0; # otherwise, keep

# phase 2:
rdmExclude$phase2Exclude[phase2passfail$total<excludeCutOff] = 1; # exclude if they missed more than 2 
rdmExclude$phase2Exclude[phase2passfail$total>=excludeCutOff] = 0; # otherwise, keep


# save rdmExclude as .csv file!
write.csv(rdmExclude, "/Volumes/CAP/data/combinedData/rdmExclusion.csv");




# EXPLORE DIFFERENT EXCLUSION CRITERIA/THRESHOLD (change the thresholds above and rerun code):
# How many participants are we excluding in phase 1?
nrow(phase1passfail[which(phase1passfail$total<=3 | phase1passfail$glm==0),]); # adding an additional criteria for glm
nrow(phase1passfail[which(phase1passfail$total<=3),]);

# How many participants are we excluding in phase 2?
nrow(phase2passfail[which(phase2passfail$total<=3 | phase2passfail$glm==0),]); # adding an additional criteria for glm
nrow(phase2passfail[which(phase2passfail$total<=3),]); 


# Some results from changing thresholds:

# missTriThresh = .1;
# pgamLowThresh = .05; 
# pgamHighThresh = .95;
# missACprop = .3; 
# minACtotal = 4; 
# phase 1: 26 people (41 if we include separate criteria for glm) 
# phase 2: 31 people (49 if we include separate criteria for glm) 

# missTriThresh = .1;
# pgamLowThresh = .03; 
# pgamHighThresh = .97;
# missACprop = .3; 
# minACtotal = 4; 
# phase 1: 24 people (41 if we include separate criteria for glm)
# phase 2: 30 people (49 if we include separate criteria for glm)

# missTriThresh = .2;  
# pgamLowThresh = .03; 
# pgamHighThresh = .97; 
# missACprop = .3; 
# minACtotal = 4; 
# phase 1: 20 people (39 if we include separate criteria for glm) 
# phase 2: 26 people (47 if we include separate criteria for glm) 

# Just change the min AC total - TLDR: results don't change much by decreasing the min AC total (change slightly when increasing it to 6)
# missTriThresh = .1; 
# pgamLowThresh = .05; 
# pgamHighThresh = .95;
# missACprop = .3; 
# minACtotal = 3; 
# phase 1: 26 people (41 if we include separate criteria for glm) 
# phase 2: 31 people (49 if we include separate criteria for glm) 
# not much different from minACtotal = 4

# missTriThresh = .1; 
# pgamLowThresh = .05; 
# pgamHighThresh = .95;
# missACprop = .3; 
# minACtotal = 2; 
# phase 1: 26 people (41 if we include separate criteria for glm) 
# phase 2: 32 people (49 if we include separate criteria for glm) 
# not much different from minACtotal = 4

# missTriThresh = .1; 
# pgamLowThresh = .05; 
# pgamHighThresh = .95;
# missACprop = .3; 
# minACtotal = 6; 
# phase 1: 23 people (41 if we include separate criteria for glm)
# phase 2: 29 people (49 if we include separate criteria for glm)
