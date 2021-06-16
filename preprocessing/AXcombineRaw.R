# Combine AX raw data for all participants on each day (saved in rawData/combinedRawData/AXday directory) and across days for each phase (saved in rawData/combinedRawData/AXphase directory) and across phases (saved in rawData/combinedRawData/)

# Hayley Brooks
# University of Denver
# created 10/30/20



# reset global environment
rm(list=ls());

# configuration
config = config::get()



# load packages
library("dplyr");                                                  
library("plyr");                                                 
library("readr");

# define variables
day = 1:20;
phase = 1:2;
pIDlength=24;

# STEP 1A: FIX DAY 3 data - COMMENTED OUT BECAUSE IT NEEDED TO BE DONE ONCE
# On day 3 of phase 1, there was an update to the Pavlovia system which lead to changes in how our script on pavlovia worked and participant IDs were not saved as they should have been on this day. For many participants, there is no participant ID in their "participant" column and an additional, empty column was added named 'X' + prolific ID (eg. X5e7ffbc0a27f256e3290542f).
# We need to do two things to fix this: 
# 1) Scrape prolific ID number from the file name and put it in each participant's csv file under the "participant" column
# 2) Then remove the additional column (eg. X5e7ffbc0a27f256e3290542f)


# the misformatted .csv files are in "/Volumes/CAP/data/rawData/phase1/day3/riskyChoiceMisformatted"
# day3AX =list.files(path = "/Volumes/CAP/data/rawData/phase1/day3/axcptMisformatted", pattern = "csv$"); # pull out the misformatted files
# extractIDax = substr(day3AX,1,pIDlength); # get list of prolific ID numbers
# nS = length(extractIDax); # number of subjects for phase 1, day 3
# 
# for (s in 1:nS) {
#   tmpcsv = read.csv(sprintf("/Volumes/CAP/data/rawData/phase1/day3/axcptMisformatted/%s", day3AX[s])); # read one sub's csv
# 
#   tmpcsv$participant = extractIDax[s]; # save prolific ID in "participant" column
# 
#   
#   # removing the additional column:
#   # possible names: "X", X + pID, rr7, sierra, prolific_cinn, bigdaddy, K, gineesh
# 
#   ind = which(names(tmpcsv) %in% c("rr7","X", "sierra", "prolific_cinn", "bigdaddy", "K", "Lizzy", "M","gineesh", sprintf('X%s',extractIDax[s])))
#   
#   if (length(ind)>0){
#     tmpcsv = tmpcsv[,-ind]; # remove it
#   };
# 
#   write.csv(tmpcsv,sprintf("/Volumes/CAP/data/rawData/phase1/day3/axcpt/%s", day3AX[s]), row.names = F); # save new .csv file
# };

# STEP 1B: fix csv file for phase 1, day 11, participant 5e89e8053cbd1167d2a5c85a.
# this prolific ID also did not get recorded correctly (participant put in their name ('Ty') instead of the participant #)
# commented out because it doesn't need to be run again
# tmpcsv = read.csv("/Volumes/CAP/data/rawData/phase1/day11/axcptMisformatted/5e89e8053cbd1167d2a5c85a_capAXCPT_2020-04-06_15h23.34.855.csv");
# tmpcsv$participant = "5e89e8053cbd1167d2a5c85a";
# ind = which(colnames(tmpcsv) == "Ty"); # which column is the funky one that needs to be removed?
# tmpcsv = tmpcsv[,-ind]; # remove it
# 
# write.csv(tmpcsv,"/Volumes/CAP/data/rawData/phase1/day11/axcpt/5e89e8053cbd1167d2a5c85a_CAPpsychopy_2020-04-06_15h01.17.219.csv", row.names = F); # save new .csv file 



# STEP 2: For each day, combine the raw AX data for each participant and save as a single large csv file (this takes several minutes to run)
for (p in phase) {
  for (d in day) {
    tmpTable <- list.files(path = sprintf("/Volumes/CAP/data/rawData/phase%d/day%d/axcpt", p, d), pattern = "*.csv", full.names = TRUE) %>%    # Get all the csv files in specified folder
      lapply(read_csv) %>%    # Store all files in list
      bind_rows               # combine all csv files
    
    tmpTable$day = d; # add day column
    tmpTable = as.data.frame(tmpTable);
    
    tmpVec =list.files(path = sprintf("/Volumes/CAP/data/rawData/phase%d/day%d/axcpt", p, d), pattern = "csv$");
    
    if (length(unique(tmpTable$participant)) == length(tmpVec)){
      file2save = sprintf("/Volumes/CAP/data/rawData/combinedRawData/AXday/collateAXphase%dday%d.csv",p,d,p,d)
      write.csv(tmpTable, file=file2save, row.names = F);
    } 
  }
};

# Note: After day 3 (phase 1) we changed the AX task so that it was split up into two parts and this changed the formatting of the data output from Pavlovia. For the collated files on days 1-3, there are 21 columns, but on days 4 onward, there are 41 columns. We will need to keep this in mind later on when we are getting the data ready for glmer.


# Which days are we missing the collated csv file (implying an error occurred)?
collateSuccess = array(data=NA, dim=list(length(day),2,2), dimnames = list(c(NULL),c("day","collate"),c(1:2)));
collateSuccess[,1,] = 1:20; # fill in days

for (p in phase) {
  for (d in day) {
    collateSuccess[d,2,p] = file.exists(sprintf("/Volumes/CAP/data/rawData/combinedRawData/AXday/collateAXphase%dday%d.csv", p,d,p,d))
  }
}; # no days are missing! (all 1s in the collate column mean success - any 0s mean there was an error for the corresponding day)




# STEP 3: combine each day into a single dataset for phase 1 and phase 2 

storeCSVnames = array(data=NA, dim=c(length(day), 1, length(phase)));

for (p in phase) {
  for (d in day) {
    storeCSVnames[d,1,p] = sprintf("/Volumes/CAP/data/rawData/combinedRawData/AXday/collateAXphase%dday%d.csv", p,d,p,d);
  }
  
  tmpTable <- storeCSVnames[,,p] %>%    # Get all the csv files for one phase
    lapply(read_csv) %>%    # Store all files in list
    bind_rows    
  
  tmpTable = as.data.frame(tmpTable); 
  tmpTable$phase = p;
  
  write.csv(tmpTable, sprintf("/Volumes/CAP/data/rawData/combinedRawData/AXphase/AXphase%d.csv", p), row.names = F); # save the csv file for each phase
};


# STEP 4: combine phase 1 and phase 2 raw data into one big .csv file

bothPhaseCSV = c("/Volumes/CAP/data/rawData/combinedRawData/AXphase/AXphase1.csv","/Volumes/CAP/data/rawData/combinedRawData/AXphase/AXphase2.csv");

tmpTable <- bothPhaseCSV %>%    # Get all the csv files for one phase
  lapply(read_csv) %>%    # Store all files in list
  bind_rows    

tmpTable = as.data.frame(tmpTable);

write.csv(tmpTable, "/Volumes/CAP/data/rawData/combinedRawData/AXall.csv", row.names = F); #save it



