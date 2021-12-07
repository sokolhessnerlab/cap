# CAP analysis files for Qualtrics survey responses
Brief description of each file in this directory listed below

Any files added to this folder should be included in this list along with the relative link (./) of that file

## 1) [SES_COVQ_PCA.RMD](analysis/Qualtrics/subLevelLong.Rdata)
- Loads scored qualtrics data for both phases
- Creates wide and long versions of individual-level dataframes
- Applies exclusion criteria for qualtrics data
- Scales the affective scored variables (e.g. pss)
- Does the SES and COVID Q PCA 
- Adds the PCA component for each person to the individual-level dataframes
- Saves individual-level dataframes as Rdata objects
  - subLevelLong.Rdata
  - subLevelWide.Rdata   


