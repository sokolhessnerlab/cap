library(plyr)
library(usmap)


# configuration
config= config::get()

#load up not scored questionaires
cap_qualtrics_not_scored_1path = file.path(config$path$combined,config$QUALTRICScsvs$Phs1_notScored_subID);
cap_qualtrics_not_scored_1 = read.csv(cap_qualtrics_not_scored_1path, stringsAsFactors = F);
#cap_qualtrics_not_scored_1 <- read.csv("~/Documents/SHLab/CAP/data/QualtricsPhase1_subID_notScored.csv", stringsAsFactors = FALSE)

cap_qualtrics_not_scored_2path = file.path(config$path$combined,config$QUALTRICScsvs$Phs2_notScored_subID);
cap_qualtrics_not_scored_2 = read.csv(cap_qualtrics_not_scored_2path, stringsAsFactors = F);

#cap_qualtrics_not_scored_2 <- read.csv("~/Documents/SHLab/CAP/data/QualtricsPhase2_subID_notScored.csv", stringsAsFactors = FALSE)

#add a phase column
cap_qualtrics_not_scored_1$phase <- 1
cap_qualtrics_not_scored_2$phase <- 2

#look at subID 461: they exist in phase 2 but not phase 1? 
count(cap_qualtrics_not_scored_1$subID == 461)
count(cap_qualtrics_not_scored_2$subID == 461)

#binds the phase 2 data to the phase 1
#note: this means that because there are a few questions asked in the first qualtrics that aren't in the second, some columns with phase = 2 have NA
cap_qualtrics_combined <- rbind.fill(cap_qualtrics_not_scored_1, cap_qualtrics_not_scored_2)

#count(is.na(cap_qualtrics_combined$demo_age))

#remove columns that are open-ended questions:

#I didn't remove these columns, but left the code in there to do so, as they seemed unnecessary
# cap_qualtrics_combined$X <- NULL
# cap_qualtrics_combined$progress <- NULL
# cap_qualtrics_combined$finished <- NULL
# cap_qualtrics_combined$distributionChannel <- NULL
# cap_qualtrics_combined$userLanguage <- NULL
cap_qualtrics_combined$covq_JAN_physicallySee <- NULL
cap_qualtrics_combined$covq_JAN_interact_q1_inPerson <- NULL
cap_qualtrics_combined$covq_JAN_interact_q2_videoChat <- NULL
cap_qualtrics_combined$covq_JAN_interact_q3_phone <- NULL
cap_qualtrics_combined$covq_JAN_interact_q4_email <- NULL
cap_qualtrics_combined$covq_JAN_interact_q5_text <- NULL
cap_qualtrics_combined$covq_JAN_interact_q6_socialMedia <- NULL
cap_qualtrics_combined$covq_NOW_physicallySee <- NULL
cap_qualtrics_combined$covq_NOW_interact_q1_inPerson <- NULL
cap_qualtrics_combined$covq_NOW_interact_q2_videoChat <- NULL
cap_qualtrics_combined$covq_NOW_interact_q3_phone <- NULL
cap_qualtrics_combined$covq_NOW_interact_q4_email <- NULL
cap_qualtrics_combined$covq_NOW_interact_q5_text <- NULL
cap_qualtrics_combined$covq_NOW_interact_q6_socialMedia <- NULL
cap_qualtrics_combined$covq_PAB_q10_socialDistanceTime <- NULL
cap_qualtrics_combined$demo_gender_other <- NULL
cap_qualtrics_combined$demo_race_other <- NULL
#cap_qualtrics_combined$demo_age <- NULL 
cap_qualtrics_combined$ses_childhood_income <- NULL
cap_qualtrics_combined$ses_childhood_householdSize <- NULL
cap_qualtrics_combined$ses_current_income <- NULL
cap_qualtrics_combined$ses_current_householdSize <- NULL
cap_qualtrics_combined$endq_unexpectedOccur_anythingElse <- NULL


#recode attention checks:
cap_qualtrics_combined$stai_attentionCheck_passed <- with(cap_qualtrics_combined, ifelse(stai_attentionCheck_select_3 == 3, 1, 0))

cap_qualtrics_combined$pss_attentionCheck_passed <- with(cap_qualtrics_combined, ifelse(pss_attentionCheck_select_1 == 1, 1, 0))

cap_qualtrics_combined$covq_attentionCheck_passed <- with(cap_qualtrics_combined, ifelse(covq_attentionCheck_select_6 == 6, 1, 0))

#some recoding of location names because they don't match the FIPS code names (and threw errors)
#note: if you use this code on another project, you might have to add some renaming because these were only renamed because they threw errors (ie participants were from those areas)
cap_qualtrics_combined$loc_state[cap_qualtrics_combined$loc_state == "Columbia"] <- "District of Columbia"

cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Anchorage" & cap_qualtrics_combined$loc_state == "Alaska"] <- "Anchorage Municipality"
cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Matanuska-Susitna" & cap_qualtrics_combined$loc_state == "Alaska"] <- "Matanuska-Susitna Borough"
cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Fairbanks North Star" & cap_qualtrics_combined$loc_state == "Alaska"] <- "Fairbanks North Star Borough"
cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Bethel" & cap_qualtrics_combined$loc_state == "Alaska"] <- "Bethel Census Area"

cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Lafayette" & cap_qualtrics_combined$loc_state == "Louisiana"] <- "Lafayette Parish"
cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "Jefferson" & cap_qualtrics_combined$loc_state == "Louisiana"] <- "Jefferson Parish"
cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "East Baton Rouge" & cap_qualtrics_combined$loc_state == "Louisiana"] <- "East Baton Rouge Parish"

cap_qualtrics_combined$loc_county[cap_qualtrics_combined$loc_county == "New York City" & cap_qualtrics_combined$loc_state == "New York"] <- "New York County"

#creates a new column for fips codes, sets it to -1 to check later if it all worked
cap_qualtrics_combined$loc_fips <- -1
for (s in 1:nrow(cap_qualtrics_combined))
{
  #DC is a unique situation, see below:
  if(cap_qualtrics_combined$loc_state[s] == "District of Columbia") 
  {
    cap_qualtrics_combined$loc_fips[s] <- 11001 # because the FIPS for DC is 11001, there is only 1 county (itself)
    #if you used the fips function here with the county info it breaks, if you don't use it, you only end up with the state code of 11 
  } 
  else #otherwise proceed with the normal fips funciton
  {
    cap_qualtrics_combined$loc_fips[s] <- fips(cap_qualtrics_combined$loc_state[s], cap_qualtrics_combined$loc_county[s])
  }
}
#checks if any places got skipped
count(cap_qualtrics_combined$loc_fips == -1) 
count(is.na(cap_qualtrics_combined$loc_fips))

#STAI Scoring:

#STAI-S individual question scoring
hToLQsSTAI_NOW <- c(1,2,5,8,10,11,15,16,19,20) #list of questions where 'Very much so' is first
ltoHQsSTAI_NOW <- c(3,4,6,7,9,12,13,14,17,18) #list of questions where 'Very much so' is last
columnOffsetSTAI_NOW <- which(colnames(cap_qualtrics_combined) == "stai_NOW_q1_calm") - 1 #gets the first stai question column number
for(c in hToLQsSTAI_NOW)
{
  c_s <- c + columnOffsetSTAI_NOW
  cap_qualtrics_combined[,c_s] <- ifelse(cap_qualtrics_combined[,c_s] == "Not at all", 4,
                                       ifelse(cap_qualtrics_combined[,c_s] == "Somewhat", 3,
                                              ifelse(cap_qualtrics_combined[,c_s] == "Moderately so", 2,
                                                     ifelse(cap_qualtrics_combined[,c_s] == "Very much so", 1, NA))))
  print(count(is.na(cap_qualtrics_combined[,c_s]))) #checks to make sure nothing was missed
}

for(c in ltoHQsSTAI_NOW)
{
  c_s <- c + columnOffsetSTAI_NOW
  cap_qualtrics_combined[,c_s] <- ifelse(cap_qualtrics_combined[,c_s] == "Not at all", 1,
                                       ifelse(cap_qualtrics_combined[,c_s] == "Somewhat", 2,
                                              ifelse(cap_qualtrics_combined[,c_s] == "Moderately so", 3,
                                                     ifelse(cap_qualtrics_combined[,c_s] == "Very much so", 4, NA))))
  print(count(is.na(cap_qualtrics_combined[,c_s]))) #checks to make sure nothing was missed
  
}

#STAI-T individual question scoring
#same as with the STAI-S, only with different question ordering
hToLQsSTAI_GEN <- c(1,3,6,7,10,13,14,16,19)
lToHQsSTAI_GEN <- c(2,4,5,8,9,11,12,15,17,18,20)
columnOffsetSTAI_GEN <- which(colnames(cap_qualtrics_combined) == "stai_GEN_q1_pleasant") - 1
for(c in hToLQsSTAI_GEN)
{
  c_t <- c + columnOffsetSTAI_GEN
  cap_qualtrics_combined[,c_t] <- ifelse(cap_qualtrics_combined[,c_t] == "Not at all", 4,
                                       ifelse(cap_qualtrics_combined[,c_t] == "Somewhat", 3,
                                              ifelse(cap_qualtrics_combined[,c_t] == "Moderately so", 2,
                                                     ifelse(cap_qualtrics_combined[,c_t] == "Very much so", 1, NA))))
  
  print(count(is.na(cap_qualtrics_combined[,c_t]))) #checks to make sure nothing was missed
}

for(c in lToHQsSTAI_GEN)
{
  c_t <- c + columnOffsetSTAI_GEN
  cap_qualtrics_combined[,c_t] <- ifelse(cap_qualtrics_combined[,c_t] == "Not at all", 1,
                                       ifelse(cap_qualtrics_combined[,c_t] == "Somewhat", 2,
                                              ifelse(cap_qualtrics_combined[,c_t] == "Moderately so", 3,
                                                     ifelse(cap_qualtrics_combined[,c_t] == "Very much so", 4, NA))))
  print(count(is.na(cap_qualtrics_combined[,c_t]))) #checks to make sure nothing was missed
}

#computing STAI scores
cap_qualtrics_combined$stai_t_score <- 0
cap_qualtrics_combined$stai_s_score <- 0
for(c in 1:20)
{
  c_s <- c + columnOffsetSTAI_NOW
  cap_qualtrics_combined$stai_s_score <- cap_qualtrics_combined$stai_s_score + cap_qualtrics_combined[,c_s]
  c_t <- c + columnOffsetSTAI_GEN
  cap_qualtrics_combined$stai_t_score <- cap_qualtrics_combined$stai_t_score + cap_qualtrics_combined[,c_t]
  
}
#double checking that everything worked out ok and is in the proper range of scores
count(cap_qualtrics_combined$stai_t_score < 20 | cap_qualtrics_combined$stai_t_score > 80)
count(cap_qualtrics_combined$stai_s_score < 20 | cap_qualtrics_combined$stai_s_score > 80)

#PSS Scoring:
reversePSS <- c(4,5,7,8) #question numbers that need reverse scoring
regularPSS <- c(1,2,3,6,9,10) #question numbers that are scored as normal
columnOffsetPSS <- which(colnames(cap_qualtrics_combined) == "pss_q1_upsetUnexpectedly") - 1
#scores the reverse questions
for(c in reversePSS)
{
  c_p <- c + columnOffsetPSS
  cap_qualtrics_combined[,c_p] <- ifelse(cap_qualtrics_combined[,c_p] == "Never", 4,
                                         ifelse(cap_qualtrics_combined[,c_p] == "Almost never", 3,
                                                ifelse(cap_qualtrics_combined[,c_p] == "Sometimes", 2,
                                                       ifelse(cap_qualtrics_combined[,c_p] == "Fairly often", 1, 
                                                              ifelse(cap_qualtrics_combined[,c_p] == "Very often", 0, NA)))))
  print(count(is.na(cap_qualtrics_combined[,c_p]))) #checks to make sure nothing was missed
  
}
#scores the regular questions
for(c in regularPSS)
{
  c_p <- c + columnOffsetPSS
  cap_qualtrics_combined[,c_p] <- ifelse(cap_qualtrics_combined[,c_p] == "Never", 0,
                                         ifelse(cap_qualtrics_combined[,c_p] == "Almost never", 1,
                                                ifelse(cap_qualtrics_combined[,c_p] == "Sometimes", 2,
                                                       ifelse(cap_qualtrics_combined[,c_p] == "Fairly often", 3, 
                                                              ifelse(cap_qualtrics_combined[,c_p] == "Very often", 4, NA)))))
  print(count(is.na(cap_qualtrics_combined[,c_p]))) #checks to make sure nothing was missed
}

#computing overall PSS score
cap_qualtrics_combined$pss_score <- 0
for(c in 1:10)
{
  c_p <- c + columnOffsetPSS
  cap_qualtrics_combined$pss_score <- cap_qualtrics_combined[,c_p] + cap_qualtrics_combined$pss_score
}

#just another double check, makes sure all the values are in the right range
count(cap_qualtrics_combined$pss_score < 0 | cap_qualtrics_combined$pss_score > 40)

#dealing with chars that generated in the response for the "today's stress rating" question
cap_qualtrics_combined$pss_stressedToday[cap_qualtrics_combined$pss_stressedToday == "1 (Not at all)"] <- 1
cap_qualtrics_combined$pss_stressedToday[cap_qualtrics_combined$pss_stressedToday == "7 (Extremely)"] <- 7

#double checking to make sure there's no other weird values
count(cap_qualtrics_combined$pss_stressedToday)

#UCLA-L Scoring:
reverseUCLA <- c(1,5,6,9,10,15,16,19,20)
regularUCLA <- c(2,3,4,7,8,11,12,13,14,17,18)
columnOffsetUCLA <- which(colnames(cap_qualtrics_combined) == "uclal_q1_inTune") - 1

#scoring the reverse questions
for(c in reverseUCLA)
{
  c_u <- c + columnOffsetUCLA
  cap_qualtrics_combined[,c_u] <- ifelse(cap_qualtrics_combined[,c_u] == "Never", 4,
                                         ifelse(cap_qualtrics_combined[,c_u] == "Rarely", 3,
                                                ifelse(cap_qualtrics_combined[,c_u] == "Sometimes", 2,
                                                       ifelse(cap_qualtrics_combined[,c_u] == "Always", 1, NA))))
  print(count(is.na(cap_qualtrics_combined[,c_u])))
  
}
#scoring for regular questions
for(c in regularUCLA)
{
  c_u <- c + columnOffsetUCLA
  cap_qualtrics_combined[,c_u] <- ifelse(cap_qualtrics_combined[,c_u] == "Never", 1,
                                         ifelse(cap_qualtrics_combined[,c_u] == "Rarely", 2,
                                                ifelse(cap_qualtrics_combined[,c_u] == "Sometimes", 3,
                                                       ifelse(cap_qualtrics_combined[,c_u] == "Always", 4, NA))))
  print(count(is.na(cap_qualtrics_combined[,c_u])))
  
}

#computing overall UCLA-L score
cap_qualtrics_combined$uclal_score <- 0
for(c in 1:20)
{
  c_u <- c + columnOffsetUCLA
  cap_qualtrics_combined$uclal_score <- cap_qualtrics_combined[,c_u] + cap_qualtrics_combined$uclal_score
}

#double checking all the final scores are in the right range
count(cap_qualtrics_combined$uclal_score < 20 | cap_qualtrics_combined$uclal_score > 80)


#deals with strings in the covq numeric responses, like the pss from before:
cap_qualtrics_combined$covq_PAB_q1_personalRisk[cap_qualtrics_combined$covq_PAB_q1_personalRisk == "1 (None)"] <- 1
cap_qualtrics_combined$covq_PAB_q1_personalRisk[cap_qualtrics_combined$covq_PAB_q1_personalRisk == "5 (Moderate)"] <- 5
cap_qualtrics_combined$covq_PAB_q1_personalRisk[cap_qualtrics_combined$covq_PAB_q1_personalRisk == "9 (Extreme)"] <- 9

cap_qualtrics_combined$covq_PAB_q2_threat[cap_qualtrics_combined$covq_PAB_q2_threat == "1 (Not a threat at all)"] <- 1
cap_qualtrics_combined$covq_PAB_q2_threat[cap_qualtrics_combined$covq_PAB_q2_threat == "7 (Extremely threatening)"] <- 7

cap_qualtrics_combined$covq_PAB_q3_personallyDie[cap_qualtrics_combined$covq_PAB_q3_personallyDie == "1 (Extremely unlikely)"] <- 1
cap_qualtrics_combined$covq_PAB_q3_personallyDie[cap_qualtrics_combined$covq_PAB_q3_personallyDie == "7 (Extremely likely)"] <- 7

cap_qualtrics_combined$covq_PAB_q4_otherPersonDie[cap_qualtrics_combined$covq_PAB_q4_otherPersonDie == "1 (Extremely unlikely)"] <- 1
cap_qualtrics_combined$covq_PAB_q4_otherPersonDie[cap_qualtrics_combined$covq_PAB_q4_otherPersonDie == "7 (Extremely likely)"] <- 7


#recoding demographic questions to numerics:

cap_qualtrics_combined$covq_PAB_q5_currentCases_recode <- with(cap_qualtrics_combined, ifelse(covq_PAB_q5_currentCases == "No", 0, 
                                                                                              ifelse(covq_PAB_q5_currentCases == "Yes", 1, 3)))
#just double checking that nothing got missed, all that changed were the labels
count(cap_qualtrics_combined$covq_PAB_q5_currentCases) 
count(cap_qualtrics_combined$covq_PAB_q5_currentCases_recode)

cap_qualtrics_combined$covq_PAB_q6_tested_recode <- with(cap_qualtrics_combined, ifelse(covq_PAB_q6_tested == "No.", 0,
                                                         ifelse(covq_PAB_q6_tested == "Yes, and it was positive.", 1,
                                                                ifelse(covq_PAB_q6_tested == "Yes, and it was negative.", 2,
                                                                       ifelse(covq_PAB_q6_tested == "Yes, and I am still waiting for results.", 3, 4)))))
count(cap_qualtrics_combined$covq_PAB_q6_tested)
count(cap_qualtrics_combined$covq_PAB_q6_tested_recode)

cap_qualtrics_combined$covq_PAB_q7_personalCovidSuspect_recode <- with(cap_qualtrics_combined, ifelse(covq_PAB_q7_personalCovidSuspect == "No, I don't think I had it or have it now.", 0,
                                                                                                      ifelse(covq_PAB_q7_personalCovidSuspect == "It's possible, but I am not sure.", 1, 2)))
count(cap_qualtrics_combined$covq_PAB_q7_personalCovidSuspect)
count(cap_qualtrics_combined$covq_PAB_q7_personalCovidSuspect_recode)


cap_qualtrics_combined$covq_PAB_q8_knowPositivePerson_recode <- with(cap_qualtrics_combined, ifelse(covq_PAB_q8_knowPositivePerson == "No", 0,
                                                                                                    ifelse(covq_PAB_q8_knowPositivePerson == "Yes", 1, 3)))
count(cap_qualtrics_combined$covq_PAB_q8_knowPositivePerson)
count(cap_qualtrics_combined$covq_PAB_q8_knowPositivePerson_recode)

cap_qualtrics_combined$covq_PAB_q9_socialDistanceLevel_recode <- with(cap_qualtrics_combined, ifelse(covq_PAB_q9_socialDistanceLevel == "No", 0,
                                                                                                     ifelse(covq_PAB_q9_socialDistanceLevel == "Somewhat", 1, 2)))
count(cap_qualtrics_combined$covq_PAB_q9_socialDistanceLevel)
count(cap_qualtrics_combined$covq_PAB_q9_socialDistanceLevel_recode)

cap_qualtrics_combined$demo_gender_recode <- with(cap_qualtrics_combined, ifelse(demo_gender == "Female", 0,
                                                                                 ifelse(demo_gender == "Male", 1,
                                                                                        ifelse(demo_gender == "Trans Female", 2,
                                                                                               ifelse(demo_gender == "Trans Male", 3,
                                                                                                      ifelse(demo_gender == "Gender nonconforming", 4, NA))))))
count(cap_qualtrics_combined$demo_gender)
count(cap_qualtrics_combined$demo_gender_recode)

cap_qualtrics_combined$demo_race_recode <- with(cap_qualtrics_combined, ifelse(demo_race == "American Indian and Alaskan Native", 0,
                                                                               ifelse(demo_race == "Asian", 1,
                                                                                      ifelse(demo_race == "Black or African American", 2,
                                                                                             ifelse(demo_race == "Native Hawaiian and Other Pacific Islander", 3,
                                                                                                    ifelse(demo_race == "White", 4,
                                                                                                           ifelse(demo_race == "Two or more races", 5,
                                                                                                                  ifelse(demo_race == "Other", 6,
                                                                                                                         ifelse(demo_race == "Decline to answer", 7, NA)))))))))
count(cap_qualtrics_combined$demo_race)
count(cap_qualtrics_combined$demo_race_recode)
                                                  
cap_qualtrics_combined$demo_ethnicity_recode <- with(cap_qualtrics_combined, ifelse(demo_ethnicity == "Not Hispanic or Latino", 0,
                                                                                    ifelse(demo_ethnicity == "Hispanic or Latino", 1, NA)))
count(cap_qualtrics_combined$demo_ethnicity)
count(cap_qualtrics_combined$demo_ethnicity_recode)

cap_qualtrics_combined$ses_childhood_freeReducedLunch_recode <- with(cap_qualtrics_combined, ifelse(ses_childhood_freeReducedLunch == "No", 0, 1))
count(cap_qualtrics_combined$ses_childhood_freeReducedLunch)
count(cap_qualtrics_combined$ses_childhood_freeReducedLunch_recode)

cap_qualtrics_combined$ses_childhood_communityComp_recode <- with(cap_qualtrics_combined, ifelse(ses_childhood_communityComp == "Less well off", 0,
                                                                                                 ifelse(ses_childhood_communityComp == "About as well off", 1,
                                                                                                        ifelse(ses_childhood_communityComp == "More well off", 2, NA))))
count(cap_qualtrics_combined$ses_childhood_communityComp)
count(cap_qualtrics_combined$ses_childhood_communityComp_recode)

cap_qualtrics_combined$ses_childhood_nationalComp_recode <- with(cap_qualtrics_combined, ifelse(ses_childhood_nationalComp == "Much less well off", 0,
                                                                                                ifelse(ses_childhood_nationalComp == "Less well off", 1,
                                                                                                       ifelse(ses_childhood_nationalComp == "About as well off", 2,
                                                                                                              ifelse(ses_childhood_nationalComp == "More well off", 3,
                                                                                                                     ifelse(ses_childhood_nationalComp == "Much more well off", 4, NA))))))
count(cap_qualtrics_combined$ses_childhood_nationalComp)
count(cap_qualtrics_combined$ses_childhood_nationalComp_recode)

cap_qualtrics_combined$ses_motherEdLevel_recode <- with(cap_qualtrics_combined, ifelse(ses_motherEdLevel == "Middle school", 0,
                                                                                       ifelse(ses_motherEdLevel == "High school", 1,
                                                                                              ifelse(ses_motherEdLevel == "Associate's degree", 2,
                                                                                                     ifelse(ses_motherEdLevel == "Bachelor's degree", 3,
                                                                                                            ifelse(ses_motherEdLevel == "Master's degree", 4,
                                                                                                                   ifelse(ses_motherEdLevel == "PhD", 5,
                                                                                                                          ifelse(ses_motherEdLevel == "JD or MD", 6, NA))))))))
count(cap_qualtrics_combined$ses_motherEdLevel)
count(cap_qualtrics_combined$ses_motherEdLevel_recode)

cap_qualtrics_combined$ses_fatherEdLevel_recode <- with(cap_qualtrics_combined, ifelse(ses_fatherEdLevel == "Middle school", 0,
                                                                                       ifelse(ses_fatherEdLevel == "High school", 1,
                                                                                              ifelse(ses_fatherEdLevel == "Associate's degree", 2,
                                                                                                     ifelse(ses_fatherEdLevel == "Bachelor's degree", 3,
                                                                                                            ifelse(ses_fatherEdLevel == "Master's degree", 4,
                                                                                                                   ifelse(ses_fatherEdLevel == "PhD", 5,
                                                                                                                          ifelse(ses_fatherEdLevel == "JD or MD", 6, NA))))))))
count(cap_qualtrics_combined$ses_fatherEdLevel)
count(cap_qualtrics_combined$ses_fatherEdLevel_recode)

cap_qualtrics_combined$ses_childhoood_homeOwnership_recode <- with(cap_qualtrics_combined, ifelse(ses_childhoood_homeOwnership == "Rent", 0,
                                                                                                  ifelse(ses_childhoood_homeOwnership == "Own", 1, NA)))
count(cap_qualtrics_combined$ses_childhoood_homeOwnership)
count(cap_qualtrics_combined$ses_childhoood_homeOwnership_recode)
                                                                                                
cap_qualtrics_combined$ses_current_billHelp_recode <- with(cap_qualtrics_combined, ifelse(ses_current_billHelp == "No", 0, 1))
count(cap_qualtrics_combined$ses_current_billHelp)
count(cap_qualtrics_combined$ses_current_billHelp_recode)

cap_qualtrics_combined$ses_current_mainResponsibilities_recode <- with(cap_qualtrics_combined, ifelse(ses_current_mainResponsibilities == "Full time student", 0,
                                                                                                      ifelse(ses_current_mainResponsibilities == "Keeping house or raising children full time", 1,
                                                                                                             ifelse(ses_current_mainResponsibilities == "Looking for work", 2,
                                                                                                                    ifelse(ses_current_mainResponsibilities == "Part time student", 3,
                                                                                                                           ifelse(ses_current_mainResponsibilities == "Retired", 4,
                                                                                                                                  ifelse(ses_current_mainResponsibilities == "Unemployed or laid off", 5,
                                                                                                                                         ifelse(ses_current_mainResponsibilities == "Working full time", 6,
                                                                                                                                                ifelse(ses_current_mainResponsibilities == "Working part time", 7, NA)))))))))
count(cap_qualtrics_combined$ses_current_mainResponsibilities)
count(cap_qualtrics_combined$ses_current_mainResponsibilities_recode)

#note a for this education level question, there were 3 blanks (?), that ended up getting recoded into NA
cap_qualtrics_combined$ses_personalEdLevel_recode <- with(cap_qualtrics_combined, ifelse(ses_personalEdLevel == "Middle school", 0,
                                                                                       ifelse(ses_personalEdLevel == "High school", 1,
                                                                                              ifelse(ses_personalEdLevel == "Associate's degree", 2,
                                                                                                     ifelse(ses_personalEdLevel == "Bachelor's degree", 3,
                                                                                                            ifelse(ses_personalEdLevel == "Master's degree", 4,
                                                                                                                   ifelse(ses_personalEdLevel == "PhD", 5,
                                                                                                                          ifelse(ses_personalEdLevel == "JD or MD", 6, NA))))))))
count(cap_qualtrics_combined$ses_personalEdLevel)
count(cap_qualtrics_combined$ses_personalEdLevel_recode)

cap_qualtrics_combined$ses_financialWorryFreq_recode <- with(cap_qualtrics_combined, ifelse(ses_financialWorryFreq == "Almost never", 0,
                                                                                     ifelse(ses_financialWorryFreq == "Once in a while", 1,
                                                                                            ifelse(ses_financialWorryFreq == "Sometimes", 2,
                                                                                                   ifelse(ses_financialWorryFreq == "Often", 3,
                                                                                                          ifelse(ses_financialWorryFreq == "Almost all the time", 4, NA))))))
count(cap_qualtrics_combined$ses_financialWorryFreq)
count(cap_qualtrics_combined$ses_financialWorryFreq_recode)

cap_qualtrics_combined$ses_needbasedCollegeAid_recode <- with(cap_qualtrics_combined, ifelse(ses_needbasedCollegeAid == "No", 0,
                                                                                             ifelse(ses_needbasedCollegeAid == "Yes", 1,3)))
count(cap_qualtrics_combined$ses_needbasedCollegeAid)
count(cap_qualtrics_combined$ses_needbasedCollegeAid_recode)

#final step: write to the .csv                                                                          
outputfilepath = file.path(config$path$combined, config$QUALTRICScsvs$Combined_subID_scored)
write.csv(cap_qualtrics_combined, outputfilepath);
#write.csv(cap_qualtrics_combined, "~/Documents/SHLab/CAP/data/QualtricsCombined_subID_scored.csv")

#note: this code outputs with all the new columns tagged on the end, for clarity, I did some post-reordering
      