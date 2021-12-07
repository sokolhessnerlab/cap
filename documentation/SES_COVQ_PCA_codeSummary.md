# A brief summary of the process and output of SES_COVQ_PCA.Rmd

1) Loads the necessary csv files including the exclusion matrices for phase 1 and phase 2 and the qualtrics data (which includes phase 1 and phase 2 responses that have been scored and recoded). There are a couple of cleaning steps here too.
2) Applies qualtrics exclusion criteria but putting in NA for those participants in the qualtrics dataframe (but keeps phase and subID as is, so that we can still find the place holders for those participants)
3) Determines the number of subjects and their IDs that were in phase 1, phase 2 and both phases
4) Creates the wide and long versions of the individual-level dataframes.
    - Wide (subLevelWide) has a row for each participant and the variables for phase 1 and 2 are distinguished by their names that begin with "phs1_<variablename>" or "phs2_<variablename>". 
      - 544 rows with 72 variables
    - Long (subLevelLong) has a row for each participant and phase (even if they didn't participate in both phases). This is so that there is a place holder for each participant and phase which was necessary for the PCA and determining which components matched with which participants
      - 1088 rows with 39 variables
  5) Scales some variables including the affective ones so that they are smaller and easier for glmer models to handle
6) Plots correlograms for SES
7) Does PCA analysis for SES and adds component 1 to individual-level dataframes
8) Plots correlograms for covid qs
9) Does PCA analysis for COVID Q
10) Save individual-level dataframes as Rdata objects 
