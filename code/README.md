# Brief summary of files in this directory
## 1.  [AXcleanData.R](./AXcleanData.R)
-   imports the large raw AX .csv file (combinedRawData/AXall.csv) and removes, renames and reformats columns so that data is basically ready for analysis
-   recodes response variables so that 1= "left" or "v" and 2 = "down" or "n"
-   replaces prolific IDs with subIDs
-   output: "/Volumes/CAP/data/combinedData/AXallClean.csv"

## 2.  [AXcombineRaw.R](./AXcombineRaw.R)

-   fixes the file format for all participants on phase 1 day 3 and one participant from phase 1 day 11 where there were issues with recording participant ID numbers. 
    -   misformatted raw data files are in rawData/phase1/day#/axcpt/axcptMisformatted/

-   combines raw axcpt data in 3 ways:
    -   across participants within each day (40 files)  
    -   across days within each phase (2 files)
    -   across both phases (1 file)

-   output: 
    -   1 .csv file per day per phase stored in the rawData/combinedRawData/combinedRawData/AXday directory (e.g. collateAXphase2day1)
    -   1 .csv file per phase (each .csv file includes data from all days) stored in combinedRawData/AXphase directory (AXphase1.csv & AXphase2.csv)
    -   1 .csv file for all the raw AX data (AXall.csv) in the rawData/combinedAXData directory
    -   Reformatted raw data files for all participants on phase 1 day 3 and one participant from phase 1 day 11 (stored in rawData/phase1/day#/axcpt).

## 3. [generateParticipantList.R](./generateParticipantList.R)

-   creates two participant lists for phase 1 and phase 2 based on the data files we have in rawData (its scrapes the prolific ID name from each file name)
-   output includes both prolific ID and CAP sub ID (the latter we will use for analyses moving forward)
-   This script also confirms that we have a file of the risky choice and axcpt task for all participants and that the prolific IDs are the same for risky choice and axcpt files.
-   output:

    -   1 .csv file per phase with columns {prolific ID, subID day, phase}

## 4. [RDMcombineRaw.R](./RDMcombineRaw.R)

-   fixes the file format for all participants on phase 1 day 3 and one participant from phase 1 day 11 where there were issues with recording participant ID numbers. 

    -   misformatted raw data files are in rawData/phase1/day#/riskyChoice/riskyChoiceMisformatted/

-   combines raw risky decision-making data in 3 ways:

    -   across participants within each day (40 files)
    -   across days within each phase (2 files)
    -   across both phases (1 file)

-   output: 

    -   1 .csv file per day per phase stored in the rawData/combinedRawData/combinedRawData/RDMday directory (e.g. collateRDMphase2day1)
    -   1 .csv file per phase (each .csv file includes data from all days) stored in combinedRawData/RDMphase directory (RDMphase1.csv & RDMphase2.csv)
    -   1 .csv file for all the raw RDM data (RDMall.csv) in the rawData/combinedRawData directory
    -   Reformatted raw data files for all participants on phase 1 day 3 and one participant from phase 1 day 11 (stored in rawData/phase1/day#/riskyChoice).

## 5. [RDMcleanData.R](./RDMcleanData.R)

-   imports the large raw RDM .csv file (combinedRawData/RDMall.csv) and removes, renames and reformats columns so that data is basically ready for analysis
-   removes the trials from the loss task in phase 1 that were incorrectly recorded due to a coding error
-   replaces prolific IDs with subIDs
-   output: "/Volumes/CAP/data/combinedData/RDMallClean.csv"

## 6. [QualtricsReplaceProlificID.R](./QualtricsReplaceProlificID.R)

-   imports the raw Qualtrics questionnaire .csv files for phase 1 and phase 2
-   cleans up the column names so that the output will only have the new variable names
-   pulls out qualtrics responses only from those who we have RDM and AXCPT data
-   removes prolific ID numbers and adds subID variable
-   saves two .csv files
-   output: "/Volumes/CAP/data/combinedData/QualtricsPhase#_subID_notScored.csv"

## 7. [QualtricsExclusion.R](./QualtricsExclusion.R)

-   loads the cleaned but not scored qualtrics .csv files: "Volumes/CAP/data/combinedData/QualtricsPhase#_subID_notScored.csv"
-   makes not of participants who should be excluded based on missed attention checks, inappropriate age responses and notes duplicate responses and missed data.
-   Only subject IDs are used here, so this script can be shared online.
-   There is no output from this file, but there is a text file that documents anomolies with qualtrics responses: "/Volumes/CAP/documentation/QualtricsExclusionCodeSummary.txt"

## 8. [RDMexclusion.R](./RDMexclusion.R)

-   loads RDM data from phase 1 and phase 2 (data/combinedData/RDMallClean.csv)
-   For each participant in each phase, reports on the 4 categories of criteria for potential exclusion

    -   miss trials, missed attention checks, p(Gamble), and subject-level glm (choice ~gain+safe)

-   output # 1: .csv file with nrow=544 (1 for each participant) with 25 variables/columns

    -   Volumes/CAP/data/combinedData/RDMqa.csv
    -   only includes subIDs (not prolific IDs)
    -   variable information for this file is located in documentation/RDMqaColumnInfo.csv

-   output # 2: .csv file with nrow=544 (1 for each participant) with 3 variables/columns

    -   Volumes/CAP/data/combinedData/rdmExclusion.csv
    -   only includes subIDs (not prolific IDs)
    -   3 columns: subID, phase1exclude (1=yes; 0=no; NA= no data), phase2exclude (1=yes; 0=no; NA=no data)

## 9. [AXexclusion.R](./AXexclusion.R)

-   loads AX data from phase 1 and phase 2 (data/combinedData/AXallClean.csv)
-   For each participants in each phase, reports whether their global performance was greater than or equal to 60%
-   output #1: AXqa.csv

    -   544 rows with 5 variables/columns
    -   only includes subIDs 
    -   5 columns: subIDs, PcntCorPhs1, MeetCritPhs1, PcntCorPhs2, MeetCritPhs2

-   output #2: axExclusion.csv

    -   544 rows with 3 variables
    -   only includes subIDs
    -   3 columns: subID, phase1exclude (1=yes; 0=no; NA= no data), phase2exclude (1=yes; 0=no; NA=no data)

## 10. [QualtricsCombineRecode.R](./QualtricsCombineRecode.R)

-   Loads phase 1 and 2 qualtrics data
-   Removes open response columns
-   Creates attentionCheck_passed columns for if a participant passed the attention check or not
-   Translates state/county responses into FIPS codes
-   Scores STAI-S, STAI-T, PSS, UCLA-L
-   Recodes non-open response covid, demographic, and ses questions to numerics

    -   Maintains the original string responses for clarity

-   Outputs QualtricsCombined_subID_scored.csv

