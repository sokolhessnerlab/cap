# Exclusion for CAP study 
# identifying those who may need to be excluded based on responses/performance on Qualtrics post-task questionnaires
# created 11/18/20 
# Hayley Brooks
# Sokol-Hessner Lab
# University of Denver

# output: 
# 1) qualtricsExclusion.csv 
# 2) QualtricsCombined_subID_scored_noDuplicates.csv


# clear environment
rm(list=ls());

nSub = 544; # we know there are 544 participants (in phase 1)

# configuration
config = config::get()

# QUALTRICS QUESTIONNAIRES
  # things to note: missed attention checks, reported age < 18 years, duplicate responses

# load qualtrics data that has subject IDs (and not prolific IDs)
# these qualtrics files have been cleaned up just a bit (include new variable names and do not include test responses or responses from people who didn't complete RDM and AXCPT task)

qPhase1csv = file.path(config$path$combined, config$QUALTRICScsvs$Phs1_notScored_subID)
qPhase1 = read.csv(qPhase1csv);
#qPhase1 = read.csv("/Volumes/CAP/data/combinedData/QualtricsPhase1_subID_notScored.csv");
# nrow = 544, ncol = 133
# length(unique(qPhase1$subID)) = 543 unique prolific IDs
# This means we have a duplicate response + missing qualtrics from someone who completed risky decision-making and AX cpt tasks

qPhase2csv = file.path(config$path$combined, config$QUALTRICScsvs$Phs2_notScored_subID);
qPhase2 = read.csv(qPhase2csv);
#qPhase2 = read.csv("/Volumes/CAP/data/combinedData/QualtricsPhase2_subID_notScored.csv");
# nrow = 357, ncol = 127
# length(unique(qPhase2$subID)) = 357 unique prolific IDs 
# We have qualtrics data from each participant in phase 2 and no duplicates)


# PHASE 1 QUALTRICS

# We have RDM and AXCPT data from 544 participants
# we have a duplicate response in our qPhase1 data and 543 unique sub IDs, which means we are missing qualtrics data from someone

# Who is the participant we are missing from qualtrics data?
which(!1:max(qPhase1$subID) %in% qPhase1$subID); #sub 461


# Who has duplicate responses?
which(diff(sort(qPhase1$subID))==0); # sub 353
# we will remove 2nd response by this subject

# Attention checks (there were 3 of these in the qualtrics questionnaires where participants were told which option to select "select 3")
# column names and expected responses:
#   "stai_attentionCheck_select_3" (correct response = 3)
#   "pss_attentionCheck_select_1"  (correct response = 1)
#   "covq_attentionCheck_select_6" (correct response = 6)

# who missed these?
ACmissedPhase1 = qPhase1$subID[qPhase1$stai_attentionCheck_select_3 !=3 | qPhase1$pss_attentionCheck_select_1 !=1 | qPhase1$covq_attentionCheck_select_6 !=6]; # save prolific IDs for those who missed attention checks

# subs 347 and 348 missed one attention check

# Age (only recorded in phase 1)
# check that we have appropriate responses for age (18+)
summary(as.numeric((qPhase1$demo_age)));
# min = 0.32  median = 28.00   mean = 30.78   max = 73.00 
# two inappropriate age responses .32 and 14

# who are they?
qPhase1$subID[qPhase1$demo_age<18];
# sub 30 (age= .32) and sub 373 (age = 14)



# PHASE 2 QUALTRICS
# We have RDM and AXCPT data from 357 participants (and 357 unique sub IDs in qPhase2 which indicates no duplicates or missing responses)


# Attention checks (there were 3 of these in the qualtrics questionnaires where participants were told which option to select "select 3")
# column names and expected responses:
#   "stai_attentionCheck_select_3" (correct response = 3)
#   "pss_attentionCheck_select_1"  (correct response = 1)
#   "covq_attentionCheck_select_6" (correct response = 6)

# who missed these?
ACmissedPhase2 = qPhase2$subID[qPhase2$stai_attentionCheck_select_3 !=3 | qPhase2$pss_attentionCheck_select_1 !=1 | qPhase2$covq_attentionCheck_select_6 !=6]; # save prolific IDs of those who missed attention checks
# sub 543 missed 1 attention check


# SUMMARY FOR EXCLUDING QUALTRICS DATA (PHASE 1 AND 2) **have not actually removed these people from any datasets yet**

# PHASE 1:
# sub 461 - missing qualtrics responses
# sub 353 - duplicate responses, will remove the second one
# sub 347 - missed an attention check (keep)
# sub 348 - missed an attention check (keep)
# sub 030 - age response = .32 (drop from all data - participated in phase2)
# sub 373 - age response = 14 (drop from all data - only participated in phase 1)

# PHASE 2: one person will be excluded in phase 2 based on age response in phase 1
# sub 030 - excluded based on age response in phase 1
# sub 543 - missed an attention check (keep)

# remove the duplicate reponse from the scored qualtrics data in phase 1 (sub 353) and save a new .csv file
qualtricsDatacsv = file.path(config$path$combined, config$QUALTRICScsvs$Combined_subID_scored)
qualtricsData = read.csv(qualtricsDatacsv);
#qualtricsData = read.csv("/Volumes/CAP/data/combinedData/QualtricsCombined_subID_scored.csv"); # load scored qualtrics data
duplicate353 = which(qualtricsData$subID==353); # find duplicate responses
qualtricsData = qualtricsData[-duplicate353[2],]; # remove the second row of data for this participant
nrow(qualtricsData[qualtricsData$phase==1,]); # check that there are now 543 rows for phase 1

qualtricsNoDuplicatesPath=file.path(config$path$combined, config$QUALTRICScsvs$Combined_subID_scored_noDuplicates)
write.csv(file=qualtricsNoDuplicatesPath,qualtricsData);
#write.csv(file="/Volumes/CAP/data/combinedData/QualtricsCombined_subID_scored_noDuplicates.csv", qualtricsData);



# Create an exclusion matrix with 3 columns: subID, phase1exclude, phase2exclude
qualtricsExclusion = data.frame(matrix(data=NA, nrow = nSub, ncol =3, dimnames=list(c(NULL), c("subID", "phase1Exclude", "phase2Exclude"))));



qualtricsExclusion$subID=1:nSub; # add subject ID numbers

phase1excludeSubID = c(30,373,461);
phase2excludeSubID = c(30);

qualtricsExclusion$phase1Exclude[phase1excludeSubID] = 1; # exclude
qualtricsExclusion$phase1Exclude[-phase1excludeSubID] = 0; # keep


phase2subIDs = qualtricsData$subID[qualtricsData$phase==2]; # subIDs for the qualtrics responses that we have for phase 2
phase2subIDs = phase2subIDs[phase2subIDs != 30]; # remove participant 30 from our sub id list

qualtricsExclusion$phase2Exclude[phase2excludeSubID]=1; #exclude

qualtricsExclusion$phase2Exclude[qualtricsExclusion$subID %in% phase2subIDs]=0; # for those we have data for in phase 2, put 0 (others will have NA)

# check it:
sum(qualtricsExclusion$phase1Exclude, na.rm = T); #should be 3
sum(qualtricsExclusion$phase2Exclude, na.rm = T); # should be 0

qualExclusionPath = file.path(config$path$combined, config$EXCLUSIONcsvs$QUALTRICS_exclusion)
write.csv(qualtricsExclusion, qualExclusionPath);
#write.csv(qualtricsExclusion, "/Volumes/CAP/data/combinedData/qualtricsExclusion.csv");

