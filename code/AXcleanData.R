# Clean and set up the raw AX-CPT data so that it is basically ready for analysis (output stored in combinedData directory)
# the output of this script will only have subIDs and not prolific IDs

# Hayley Brooks
# University of Denver
# created 11/5/20

# reset global environment
rm(list=ls());


# STEP 1: Load raw AX data file (all participants across both phases) and participant ID files and define some variables
rawAX = read.csv("/Volumes/CAP/data/rawData/combinedRawData/AXall.csv");

subNums = unique(rawAX$participant);
nSub = length(subNums); 

# Most participants will have 120 real trials per phase. Starting on day 6, we switched the key press from "left" and "down" to "v" and "n" to be consistent with the risky decision-making task. We also included 5 practice trials on day 6 and beyond (so for those participants there are 126 trials). For each participant in each phase, Pavlovia saves the last row as exit responses, so we can get rid of this row for each participant.


# STEP 2: Pull out the columns from raw data that we need to move forward
keepCols = c("getResponse.keys", "getResponse.rt","firstLetter","secondLetter","participant", "day", "phase");
tmpDF = rawAX[keepCols]; # 7 columns


# STEP 3: Make our massive clean dataset for AX data
# For each participant:
# 1) remove practice trials {done}
# 2) create new trial number variable, start from 1 instead of 0 {done}
# 3) remove the last row in each phase
# 5) store each participant's data so that phase 1 and phase 2 are together for a single participant (at this point, all the phase 1 data is first, followed by phase 2 and we want the dataset to be organized by participant, not phase) {done}
# 6) rename columns {done}
# 7) recode response variable to be 1 = ("left" or "v") and 2 = ("down" or "n") {done}
# 8) add correct response column (1=hit; 0=miss) {done}
# 9) add sub IDs and remove the prolific IDs {done}

# set up the output variable
cleanCols = c("axResponse","axRT","axFirstLetter", "axSecondLetter","subID", "day","phase", "axTrial"); # 8 columns
cleanAX = matrix(data=NA, ncol = length(cleanCols), dimnames=list(c(NULL),cleanCols));

for (s in 1:nSub) { 
  
  sub = tmpDF[tmpDF$participant==subNums[s],]; # pull out single participant
  
  # PHASE 1:
  phs1 = sub[sub$phase==1,]; # pull out phase 1 data
  
  #remove practice trials
  if (phs1$day[1] >=6){
    phs1 = phs1[-c(1:5),]; 
  };
  
  # create trial variable
  phs1$trial = NA;
  trialRealInd = which(!is.na(phs1$firstLetter));
  phs1$trial[trialRealInd] = 1:length(trialRealInd);
  
  phs1 = phs1[-which(is.na(phs1$trial)),]; # remove the last row (it doesn't contain valuable info for analysis)

  
  # If participant completed phase 2:
  if(length(unique(sub$phase))>1){
    phs2 = sub[sub$phase==2,];
    
    # remove practice trials
    phs2 = phs2[-c(1:5),]; 
   
    
    # create trial variable
    phs2$trial = NA;
    trialRealInd = which(!is.na(phs2$firstLetter));
    phs2$trial[trialRealInd] = 1:length(trialRealInd);
    
    phs2 = phs2[-which(is.na(phs2$trial)),]; # remove the last row (it doesn't contain valuable info for analysis)
    
    tmpSub = rbind(phs1, phs2); # combine phase 1 and phase 2 for participant s
    
  } else {
    tmpSub = phs1; # if participant completed phase 1 only
  }; # end if(length(unique(sub$phase))>1) loop
  
  tmpSub$participant = s; # change the prolific ID numbers to subIDs

  # rename columns
  colnames(tmpSub) = cleanCols;

  # add tmpSub to cleanAX
  cleanAX = rbind(cleanAX, tmpSub);
  
}; # end for s in 1:nSub loop

# remove the first row  (all NAs) in the cleanAX matrix;
cleanAX = cleanAX[2:nrow(cleanAX),];

# Recode the response variable so that 1 = "left" or "v"; 2 = "down" or "n"
cleanAX$axResponse[cleanAX$axResponse=="left" | cleanAX$axResponse == "v"] = 1;
cleanAX$axResponse[cleanAX$axResponse=="down" | cleanAX$axResponse == "n"] = 2;
cleanAX$axResponse = as.numeric(cleanAX$axResponse);

# Add a variable for correct and incorrect responses
letter1 = unique(cleanAX$axFirstLetter); # all possible first letters
letter2 = unique(cleanAX$axSecondLetter); # all possible second letters
letter1noA = unique(cleanAX$axFirstLetter[cleanAX$axFirstLetter !="A"]); # possible first letters other than A
letter2noX = unique(cleanAX$axSecondLetter[cleanAX$axSecondLetter !="X"]); # possible second letters other than X

cleanAX$axCorrect = NA; # add column
# for AX trials
cleanAX$axCorrect[cleanAX$axFirstLetter == "A" & cleanAX$axSecondLetter== "X" & cleanAX$axResponse==1] = 1; # sub said AX = correct
cleanAX$axCorrect[cleanAX$axFirstLetter == "A" & cleanAX$axSecondLetter== "X" & cleanAX$axResponse==2] = 0; # sub said other = incorrect

# any other combination of letters
cleanAX$axCorrect[cleanAX$axFirstLetter %in% letter1 & cleanAX$axSecondLetter %in% letter2noX & cleanAX$axResponse==1] = 0; # first letter could have been A, but was not followed by X, but sub said AX = incorrect
cleanAX$axCorrect[cleanAX$axFirstLetter %in% letter1noA & cleanAX$axSecondLetter %in% letter2 & cleanAX$axResponse==1] = 0; # second letter could have been X, but first letter was not A, but sub said AX = incorrect

cleanAX$axCorrect[cleanAX$axFirstLetter %in% letter1 & cleanAX$axSecondLetter %in% letter2noX & cleanAX$axResponse==2] = 1; # first letter could have been A, but was not followed by X and sub said other = correct
cleanAX$axCorrect[cleanAX$axFirstLetter %in% letter1noA & cleanAX$axSecondLetter %in% letter2 & cleanAX$axResponse==2] = 1; # second letter could have been X, but first letter was not A and sub said other = correct





# STEP 4: SAVE THE CLEAN DATA
write.csv(cleanAX, "/Volumes/CAP/data/combinedData/AXallClean.csv", row.names = F); # save as .csv file
# cleanAX has 108,091 rows and 8 columnes (each participant has either ~120 trials (if completed phase 1 only) or ~240 trials (if completed both phases))


