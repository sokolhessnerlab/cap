# Create participant .csv files for phase 1 and phase 2 of CAP study
# output will include 4 columns: prolific ID, CAP sub ID, day, phase

# Hayley Brooks
# University of Denver
# created 10/27/20

# reset global environment
rm(list=ls());


# define some variables
pIDlength = 24;
day = 1:20;
phase = 1:2;


# Check that we have the axcpt and risky choice data for each participant on each day and that proflific IDs match up
checkMatch = array(dim = list(length(day),2,length(phase)), dimnames = list(c(NULL), c("match","day"), phase));
checkMatch[,2,] = day;

for (p in phase) {
  for (d in day) {
    tmpVecAX =list.files(path = sprintf("/Volumes/CAP/data/rawData/phase%d/day%d/axcpt", p, d), pattern = "csv$");
    tmpVecDM =list.files(path = sprintf("/Volumes/CAP/data/rawData/phase%d/day%d/riskyChoice", p, d), pattern = "csv$");
    
    if (as.numeric(length(tmpVecAX) == length(tmpVecDM))){ # if there is the same number of axcpt and risky choice files for day[d]
      extractIDax = substr(tmpVecAX,1,pIDlength);
      extractIDdm = substr(tmpVecDM,1,pIDlength);
      
      tmp = vector(length = length(extractIDax));
      
      for (s in 1:length(extractIDax)) {
        tmp[s] = as.numeric(extractIDax[s] == extractIDdm[s]); # check that prolific IDs are the same for axcpt and risky choice files
      }
      
      checkMatch[d,1,p] = sum(tmp[d]!=1, na.rm = T); # 0 in the match column implies no errors
      
    }else{
      sprintf("directory size different for day %d in phase %d", day[d], phase[p])
    }
  };
};

sum(checkMatch[,1,]);# Should be 0 - All prolific IDs match up across axcpt and risky choice files (value > 0 implies error)


#  Create and save two text files (one file for each phase) with columns {prolific IDs, day, phase} 

for (p in phase){
  participantIDs=matrix(data=NA, ncol = 3, dimnames = list(c(NULL), c("prolificID", "day", "phase")));
  
  for (d in day) {
    # prolific IDs match up across axcpt and risky choice data, so we can just pull the prolific ID numbers from either axcpt or risky choice files
    tmpVec =list.files(path = sprintf("/Volumes/CAP/data/rawData/phase%d/day%d/axcpt", p, d), pattern = "csv$");
    extractID = substr(tmpVec,1,pIDlength);
    tmpMat = matrix(data=NA, nrow = length(tmpVec), ncol = 3, dimnames = list(c(NULL), c("prolificID", "day", "phase")));
    tmpMat[,] = c(extractID, rep(d, times=length(extractID)),rep(p, times=length(extractID)));
    
    participantIDs = rbind(participantIDs,tmpMat);
  };
  
  participantIDs = participantIDs[-1,]; # remove first row of participantIDs (its a row of NA)
  
  write.csv(participantIDs, file=sprintf('/Volumes/CAP/data/combinedData/phase%d_participant.csv', p), quote=FALSE, row.names = F); # save .csv files
};


# update 11/12/20: Add subIDs to the our participant.csv files 
# for phase 1, the sub IDs will be 1-544
# for phase 2, sub IDs will be 3-544 and there will be gaps since all participants did not complete phase 2
# there are probably more sophisticated ways to do this :)

# load phase 1 participant list
p1ID = read.csv("/Volumes/CAP/data/combinedData/phase1_participant.csv");
p1ID = cbind(p1ID, 1:nrow(p1ID)); # add subID 1-544
names(p1ID)[4] = "subID"; # rename column for subID

p2ID = read.csv("/Volumes/CAP/data/combinedData/phase2_participant.csv");
p2ID = cbind(p2ID, NA); # create a new column of NAs
names(p2ID)[4] = "subID"; # add column name
for (r in 1:nrow(p2ID)) { # for all participants in phase 2
  p2ID[r,4] = p1ID[p1ID[,1] == p2ID[r,1],4]; # find their sub ID number from phase 1 and save it in the phase 2 table
};

write.csv(p1ID, file='/Volumes/CAP/data/combinedData/phase1_participant.csv', quote=FALSE, row.names = F); # save .csv files
write.csv(p2ID, file='/Volumes/CAP/data/combinedData/phase2_participant.csv', quote=FALSE, row.names = F); # save .csv files


