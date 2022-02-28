#clear environment
rm(list=ls())

#source all scripts
source("001_setup.R")
source("002_age_conversion.R")
source("003_sex_spayed_neutered_separation.R")
source("004_binary_euth_status.R")
source("005_month_season_conversion.R")
source("006_danger_zones.R")
source("007_final_data.R")
source("008_log_reg.R")
source("009_knn.R")
source("011_rf_final.R")
source("012_training_ROCs.R")
