# Exclusion for CAP study 
# identifying those who may need to be excluded based on responses/performance on Qualtrics post-task questionnaires
# created 11/18/20 Hayley Brooks

rm(list=ls());

nSub = 544; # we know there are 544 participants (in phase 1)

# QUALTRICS QUESTIONNAIRES
  # things to note: missed attention checks, reported age < 18 years, duplicate responses

# load qualtrics data that has subject IDs (and not prolific IDs)
# these qualtrics files have been cleaned up just a bit (include new variable names and do not include test responses or responses from people who didn't complete RDM and AXCPT task)
qPhase1 = read.csv("/Volumes/CAP/data/combinedData/QualtricsPhase1_subID_notScored.csv");
# nrow = 544, ncol = 132
# 543 unique prolific IDs which means we have a duplicate response + are missing qualtrics from someone who completed the risky decision-making and AX cpt tasks

qPhase2 = read.csv("/Volumes/CAP/data/combinedData/QualtricsPhase2_subID_notScored.csv");
# nrow = 357, ncol = 126
# 357 unique prolific IDs (implying that we have qualtrics data from each participant in phase 2 and no duplicates)


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

# PHASE 2:
# sub 543 - missed an attention check (keep)

