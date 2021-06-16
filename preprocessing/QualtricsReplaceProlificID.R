# The qualtrics questionnaires for phase 1 and phase 2 include prolific IDs and we need them to have sub IDs
# this script loads the raw qualtrics files (CAP/data/rawData/phase#/qualtrics2phase#.csv) and
#   1) removes the rows with the old column names (will just include new column names)
#   2) pulls out the qualtrics responses from participants included in the phase#_participant.csv files 
#   3) replace the prolific IDs with subject IDs 
#   4) save new qualtrics files 
  # only qualtrics responses for those we have RDM and AXCPT data with their subject Ids instead of prolific IDS

# created 11/24/20 HRB


# clean environment
rm(list=ls());

# configuration
config = config::get();

# load files
qPhase1path = file.path(config$path$raw, config$QUALTRICScsvs$Phs1_raw);
qPhase1 =read.csv(qPhase1path);

qPhase2path = file.path(config$path$raw, config$QUALTRICScsvs$Phs2_raw);
qPhase2 = read.csv(qPhase2path);

phase1subspath = file.path(config$path$combined, config$SUBcsvs$phase1_participant);
phase1subs = read.csv(phase1subspath);

phase2subspath = file.path(config$path$combined, config$SUBcsvs$phase2_participant);
phase2subs = read.csv(phase2subspath);

#qPhase1 = read.csv('/Volumes/CAP/data/rawData/phase1/qualtricsResponses/qualtrics2phase1.csv');
#qPhase2 = read.csv('/Volumes/CAP/data/rawData/phase2/qualtricsResponses/qualtrics2phase2.csv');
#phase1subs = read.csv('/Volumes/CAP/data/combinedData/phase1_participant.csv');
#phase2subs = read.csv('/Volumes/CAP/data/combinedData/phase2_participant.csv');


# STEP 1: NEW COLUMN NAMES 
## Note: The column names and second row in these csv files contain the original variable names from Qualtrics that are not helpful (e.g. Q_17_7).The third row is the new variable names (e.g. covq_PAB_q9_socialDistanceLevel).

# rename the column names of the qualtrics dataframes to be our new variable names (stored in the 3rd row)
colnames(qPhase1) = qPhase1[3,];
colnames(qPhase2) = qPhase2[3,];

# remove the first 3 rows that are just variable names
qPhase1 = qPhase1[-c(1:3),];
qPhase2 = qPhase2[-c(1:3),];

# STEP 2: PULL RESPONSES FROM PEOPLE WE HAVE RDM AND AXCPT DATA FOR 
qPhase1keep = qPhase1[qPhase1$prolificID %in% phase1subs$prolificID,]; 
qPhase2keep = qPhase2[qPhase2$prolificID %in% phase2subs$prolificID,]; 


# STEP 3: REPLACE PROLIFIC IDS WITH SUBJECT IDS
qPhase1keep$subID = NA;
qPhase2keep$subID = NA;

# phase 1:
for (s in 1:nrow(qPhase1keep)) {
  qPhase1keep$subID[s]=phase1subs$subID[phase1subs$prolificID == qPhase1keep$prolificID[s]]
};

# phase 2:
for (s in 1:nrow(qPhase2keep)) {
  qPhase2keep$subID[s]=phase2subs$subID[phase2subs$prolificID == qPhase2keep$prolificID[s]]
};

# remove prolific ID column
qPhase1keep = qPhase1keep[,-which(names(qPhase1keep) %in% c("prolificID"))];
qPhase2keep = qPhase2keep[,-which(names(qPhase2keep) %in% c("prolificID"))];


# STEP 4: SAVE NEW QUALTRICS FILES IN COMBINED DATA
qPhase1keepPath = file.path(config$path$combined, config$QUALTRICScsvs$Phs1_notScored_subID);
write.csv(qPhase1keep, qPhase1keepPath);
qPhase2keepPath = file.path(config$path$combined, config$QUALTRICScsvs$Phs2_notScored_subID);
write.csv(qPhase2keep, qPhase2keepPath);

#write.csv(qPhase1keep, "/Volumes/CAP/data/combinedData/QualtricsPhase1_subID_notScored.csv");
#write.csv(qPhase2keep, "/Volumes/CAP/data/combinedData/QualtricsPhase2_subID_notScored.csv")
