# Add SES PCA, scaled affective variables and covid risk variables (scaled, scaledNoNA) to AX data.
# Hayley Brooks, University of Denver, 12/7/2021

# load packages
library('config')

# configuration
config = config::get()

# set up file paths
ax_csv = file.path(config$path$combined, config$AXcsvs$AX_qualtrics); # set file path for AX + qualtrics data
subLevelLong_path = file.path(config$path$Rdata, config$Rdata_files$QualtricsSubLevelLong); # subject-level long format path

# load csvs and Rdata files
axQualtrics = read.csv(ax_csv); # load ax + qualtrics data
load(subLevelLong_path); # load subject-level long dataframe

# Remove the extra "X" column present as the first column in datasets
axQualtrics = axQualtrics[,(2:ncol(axQualtrics))];

# add the new variables to the AX dataset
for(s in 1:nrow(subLevelLong)){
  axQualtrics$sesPCA[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$sesPCA[s];
  
  axQualtrics$stai_s_score_scaled[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$stai_s_score_scaled[s];
  axQualtrics$stai_t_score_scaled[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$stai_t_score_scaled[s];
  axQualtrics$uclal_score_scaled[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$uclal_score_scaled[s];
  axQualtrics$pss_score_scaled[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$pss_score_scaled[s];
  
  axQualtrics$covq_PAB_q1_personalRisk_scaled[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaled[s];
  axQualtrics$covq_PAB_q1_personalRisk_scaledNoNA[subLevelLong$subID[s]==axQualtrics$subID & subLevelLong$phase[s] == axQualtrics$phase] = subLevelLong$covq_PAB_q1_personalRisk_scaledNoNA[s];

};



save(file =file.path(config$path$Rdata,config$Rdata_files$AX_qualtrics_PCA),axQualtrics); # save this as "/Volumes/CAP/Rdata/axQualtricsPCA.Rdata"

