# Clean and set up the raw risky decision-making data so that it is basically ready for analysis (output stored in combinedData directory)
# remove the messed up trials from the loss task in phase 1 (see notes below), keep missed trials, and do not exclude anyone
# output of this script includes subIDs not prolific IDs

# Hayley Brooks
# University of Denver
# created 11/1/20

# reset global environment
rm(list=ls());


# STEP 1: Load raw RDM data file (all participants across both phases) and define some variables
rawRDM = read.csv("/Volumes/CAP/data/rawData/combinedRawData/RDMall.csv");

subNums = unique(rawRDM$participant);
nSub = length(subNums); 

# IMPORTANT NOTES ABOUT THE RAW DATASET
# Each participant has 144 trials for each day they participated (practice trials: 1-5; gain-only trials: 6-124; loss trials: 125 - 144)
# For participants who were completed phase 1 and 2, there are 288 trials, but the dataset is organized by phase and not participant 


# STEP 2: There was a coding error in the loss task in phase 1 where outcomes were not being recorded correctly when participants selected the safe option when the safe option is on the right (rawRDM$loc = 1). We need to remove those trials.
errorInd = which(rawRDM$realChoice_2.keys=='n' & rawRDM$loc==1 & rawRDM$outcomeType !='safe'); # 667 of the loss trials (6.1% of loss trials)

rawRDM = rawRDM[-errorInd,]; # now we have 129077 rows


# STEP 3: Pull out the columns from raw data that we need to move forward
keepCols = c("riskyGain", "riskyLoss", "alternative", "isi", "iti", "participant","extraITIreal","realChoice.rt","realOutcome","choice","realTrials.thisTrialN","groundEV","evInd","runSize","realChoice_2.rt","outcomeType","lossChoice","lossTrials.thisTrialN","day","phase");

tmpDF = rawRDM[keepCols]; # 20 columns


# STEP 4: Make our massive clean dataset for RDM data
# For each participant:
# 1) remove practice trials {done} 
# 2) add task column (gain-only = 1, loss = 2) {done}
# 3) combine realChoice.rt and realChoice_2.rt {done}
# 4) combine choice and loss choice {done}
# 5) create trial number variable (1:119 for gain; 1-20 for loss) {done}
# 6) outcomeType only exists for loss and is a character vector: "win", "loss", "safe", so:
      #4a) code the loss task trials to be 0 (safe),1 (gain),-1 (loss) {done}
      #4b) code this for gain-only trials too {done}
# 7) make riskyLoss and loss outcomes negative values (only for days 1-18 on phase 1) {done}
# 8) delete the extra loss columns that we don't need anymore ("realTrials.thisTrialN", "realChoice_2.rt","lossChoice", "lossTrials.thisTrialN")
# 9) store each participants data so that phase 1 and phase 2 are together for a single participant (at this point, all the phase 1 data is first, followed by phase 2 and we want the dataset to be organized by participant, not phase)
# 10) rename 3 columns ("extraITIreal","realChoice.rt", "realOutcome")

# set up the output variable
cleanCols = c("rdmRiskyGain","rdmRiskyLoss","rdmAlternative", "rdmIsi", "rdmIti","subID","rdmItiExtra", "rdmRT","rdmOutcome", "rdmChoice", "rdmGroundEV","rdmEvInd","rdmRunSize","rdmOutcomeType", "day","phase","rdmTask","rdmTrial")
cleanRDM = matrix(data=NA, ncol = 18, dimnames=list(c(NULL),cleanCols))

for (s in 1:nSub) { 
  
  sub = tmpDF[tmpDF$participant==subNums[s],]; # pull out single participant
  
  # PHASE 1:
  phs1 = sub[sub$phase==1,]; # pull out phase 1 data
  phs1 = phs1[-c(1:5),]; # remove practice trials
  
  # create task variable (gain-only = 1; loss = 2)
  phs1$task = NA;
  phs1$task[is.finite(phs1$realTrials.thisTrialN)] = 1; 
  phs1$task[is.finite(phs1$lossTrials.thisTrialN)] = 2; 
  
  
  phs1$realChoice.rt[phs1$task==2] = phs1$realChoice_2.rt[phs1$task==2]; # combine RT  variable across gain and loss task
  phs1$choice[phs1$task==2] = phs1$lossChoice[phs1$task==2]; # combine choice variable across gain and loss task
  
  # create trial variable
  phs1$trial = NA;
  phs1$trial[phs1$task==1] = c(1:length(which(phs1$task==1))); # gain-only task
  phs1$trial[phs1$task==2] = c(1:length(which(phs1$task==2))); # loss task
  
  # recode outcomeType from loss task
  phs1$outcomeType[phs1$outcomeType=="safe"] = 0;
  phs1$outcomeType[phs1$outcomeType=="win"] = 1;
  phs1$outcomeType[phs1$outcomeType=="loss"] = -1;
  phs1$outcomeType = as.numeric(phs1$outcomeType);
  
  # code outcomeType for gain-only trials
  phs1$outcomeType[phs1$task==1 & phs1$realOutcome==phs1$alternative] = 0;
  phs1$outcomeType[phs1$task==1 & phs1$realOutcome==phs1$riskyGain] = 1;
  phs1$outcomeType[phs1$task==1 & phs1$realOutcome==phs1$riskyLoss] = -1;
  
  # for days 1-18 in phase 1, riskyLoss were all positive values - make them negative
  if(phs1$day[1] <=18){
    phs1$riskyLoss = phs1$riskyLoss*-1;
  }
  
  # If participant completed phase 2:
  if(length(unique(sub$phase))>1){
    phs2 = sub[sub$phase==2,];
    
    phs2 = phs2[-c(1:5),]; # remove practice trials
    
    phs2$task = NA;
    phs2$task[is.finite(phs2$realTrials.thisTrialN)] = 1; 
    phs2$task[is.finite(phs2$lossTrials.thisTrialN)] = 2; 
    
    phs2$realChoice.rt[phs2$task==2] = phs2$realChoice_2.rt[phs2$task==2];
    phs2$choice[phs2$task==2] = phs2$lossChoice[phs2$task==2];
    
    phs2$trial = NA;
    phs2$trial[phs2$task==1] = c(1:length(which(phs2$task==1)));
    phs2$trial[phs2$task==2] = c(1:length(which(phs2$task==2)));
    
    
    phs2$outcomeType[phs2$outcomeType=="safe"] = 0;
    phs2$outcomeType[phs2$outcomeType=="win"] = 1;
    phs2$outcomeType[phs2$outcomeType=="loss"] = -1;
    phs2$outcomeType = as.numeric(phs2$outcomeType);
    
    phs2$outcomeType[phs2$task==1 & phs2$realOutcome==phs2$alternative] = 0;
    phs2$outcomeType[phs2$task==1 & phs2$realOutcome==phs2$riskyGain] = 1;
    phs2$outcomeType[phs2$task==1 & phs2$realOutcome==phs2$riskyLoss] = -1;
    
    tmpSub = rbind(phs1, phs2); # combine phase 1 and phase 2 for participant s
  } else {
    tmpSub = phs1; # if participant completed phase 1 only
  }; # end if(length(unique(sub$phase))>1) loop
  
  # remove columns we don't need anymore
  deleteCols = c("realTrials.thisTrialN", "realChoice_2.rt","lossChoice", "lossTrials.thisTrialN");
  tmpSub = tmpSub[,!(names(tmpSub) %in% deleteCols)];
  
  # rename columns
  colnames(tmpSub) = cleanCols;
  
  tmpSub$subID = s; # replace prolific ID with subID
  
  # add tmpSub to cleanRDM
  cleanRDM = rbind(cleanRDM, tmpSub);
  
}; # end for s in 1:nSub loop

# remove the first row  (all NAs) in the cleanRDM matrix;
cleanRDM = cleanRDM[2:nrow(cleanRDM),];


write.csv(cleanRDM, "/Volumes/CAP/data/combinedData/RDMallClean.csv", row.names = F); # save as .csv file
# cleanRDM has 124,572 rows and 18 columns (each participant has either ~139 trials (completed phase 1 only) or ~278 trials (completed both phases))


