##########################################################
# 00_run_project.R
#
# Purpose: Reproduce the entire NHL Breakout Forward Prediction Project
#
# Running this script will execute every step of the analysis in the correct order.
#
##########################################################

library(here)

message("Running 01_download_and_clean_data.R")
source(here("code", "01_download_and_clean_data.R"))
message("Running 02_feature_engineering.R")
source(here("code", "02_feature_engineering.R"))
message("Running 03_build_model.R")
source(here("code", "03_build_model.R"))
message("Running 04_model_selection.R")
source(here("code", "04_model_selection.R"))
message("Running 05_model_evaluation.R")
source(here("code", "05_model_evaluation.R"))
message("Running 06_case_studies_part_one.R")
source(here("code", "06_case_studies_part_one.R"))
message("Running 07_case_studies_part_two.R")
source(here("code", "07_case_studies_part_two.R"))
message("Running 08_predictions.R")
source(here("code", "08_predictions.R"))

message("Project completed successfully!")

##########################################################
# End of script
##########################################################