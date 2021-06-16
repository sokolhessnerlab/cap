# Exclusion for CAP study 
# Identifying those who may need to be excluded from AX-CPT task
# OUTPUT: two .csv files (CAP/data/combinedData/)
# 1) AXqa.csv
# 2) axExclusion.csv

# Criteria for exclusion (by participant) based on AX data
# participants will be excluded if their global performance is less than 60%

# created 4/29/21 Hayley Brooks

# reset environment
rm(list=ls()); 

# configuration
config = config::get()

# load ax data (clean, combined data across phases with sub IDs)
axcsv = file.path(config$path$combined,config$AXcsvs$AX_allClean);
ax = read.csv(axcsv);# takes ~5 seconds to load
#ax = read.csv("/Volumes/CAP/data/combinedData/AXallClean.csv"); 

subNums = unique(ax$subID); # unique sub ID numbers
nSub = length(subNums); # number of participants


# Threshold for inclusion is >= 60%
# ax dataset contains the column "axCorrect" where 1 = correct; 0 = incorrect; NA = no response

# calculate performance for each participant, store in a dataframe that includes subID, percent correct for each phase, whether they meet criteria for keeping (1 = keep, 0 = exclude) for each phase, save as AXqa.csv file
AXqa =data.frame(matrix(data=NA, nrow=nSub, ncol=5, dimnames = list(c(NULL),c("subID", "PcntCorPhs1", "MeetCritPhs1","PcntCorPhs2", "MeetCritPhs2")))); # create the dataframe
AXqa$subID = subNums; # fill in subID

for (s in 1:length(subNums)) {
  AXqa$PcntCorPhs1[s] = sum(ax$axCorrect[ax$subID==subNums[s] & ax$phase==1], na.rm = T)/length(ax$axCorrect[ax$subID==subNums[s] & ax$phase==1]);
  
  #if participant completed phase 2:
  if(any(ax$phase[ax$subID==subNums[s]]==2)){
    AXqa$PcntCorPhs2[s] = sum(ax$axCorrect[ax$subID==subNums[s] & ax$phase==2], na.rm = T)/length(ax$axCorrect[ax$subID==subNums[s] & ax$phase==2]);
  } # end if statement
  
}; # if for loop


# keep participants that performed at or equal to .6 (60%), otherwise, we will exclude them
AXqa$MeetCritPhs1[AXqa$PcntCorPhs1>=.6] = 1;
AXqa$MeetCritPhs1[AXqa$PcntCorPhs1<.6] = 0;

AXqa$MeetCritPhs2[AXqa$PcntCorPhs2>=.6] = 1;
AXqa$MeetCritPhs2[AXqa$PcntCorPhs2<.6] = 0;

# save AXqa dataframe
AXqaPath = file.path(config$path$combined,config$AXcsvs$AX_qa);
write.csv(AXqa,AXqaPath);
#write.csv(AXqa, "/Volumes/CAP/data/combinedData/AXqa.csv");


# Make the exclusion .csv file (subID, phase1Exclude, phase2Exclude)
# how many people met inclusion criteria?
# Phase 1:
sum(AXqa$MeetCritPhs1==1, na.rm=T); # 522 (excluding 22)
# Phase 2:
sum(AXqa$MeetCritPhs2==1, na.rm=T); # 351 (excluding 6)

axExclusion = data.frame(matrix(data=NA, nrow = nSub, ncol =3, dimnames=list(c(NULL), c("subID", "phase1Exclude", "phase2Exclude"))));

axExclusion$subID=subNums; # add subject ID numbers
axExclusion$phase1Exclude[AXqa$MeetCritPhs1==1]=0; # phase 1 keep
axExclusion$phase2Exclude[AXqa$MeetCritPhs2==1]=0; # phase 2 keep
axExclusion$phase1Exclude[AXqa$MeetCritPhs1==0]=1; # phase 1 exclude
axExclusion$phase2Exclude[AXqa$MeetCritPhs2==0]=1; # phase 2 exclude

# check it:
sum(axExclusion$phase1Exclude, na.rm = T); #should be 22
sum(axExclusion$phase2Exclude, na.rm = T); # should be 6

# save axExclusion dataframe as csv file
axExclusionPath = file.path(config$path$combined, config$EXCLUSIONcsvs$AX_exclusion);
write.csv(axExclusion, axExclusionPath);
#write.csv(axExclusion, "/Volumes/CAP/data/combinedData/axExclusion.csv");
