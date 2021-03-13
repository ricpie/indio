#Load functions
source('thresholds-master/UPAS_thresholds.R')
source('thresholds-master/lascar_thresholds.R')
source('thresholds-master/ecm_thresholds.R')
source('r_scripts/UPAS_functions.R')
source('r_scripts/lascar_functions.R')
source('r_scripts/ecm_functions.R')
source('r_scripts/pats_functions.R')
source('r_scripts/merging_functions.R')
source('r_scripts/beacon_functions.R')
source('r_scripts/generic_functions.R')
source('r_scripts/ses_pca_function.R')
source('r_scripts/tsi_functions.R')
source('r_scripts/plots.r')
mobenzi_import_fun()
equipment_IDs_fun()
lascar_cali_import()
local_tz = "Africa/Nairobi"


#Load paths
path_other <- "../Data/Data from the team/Excel databases/Other databases.xlsx"
path_emissions <- "../Data/Data from the team/Excel databases/2019 E2E Emissions database_v2.xlsx"

tsifilepath <- "Processed Data/Cleaned TSI Data" #the corrected files should be placed here.
file_list_tsi <- list.files(tsifilepath, pattern='.csv|.CSV', full.names = T,recursive = T)

upasfilepath <- "../Data/Data from the team/UPAS" 
file_list_upas <- list.files(upasfilepath, pattern='.txt|.TXT', full.names = T,recursive = F)

patsfilepath <- "../Data/Data from the team/PATS+/HAP P+"
file_list_pats <- list.files(patsfilepath, pattern='.csv|.CSV', full.names = T,recursive = F)

lascarfilepath <- "../Data/Data from the team/Lascar/Data Files" 
file_list_lascar <- list.files(lascarfilepath, pattern='.txt|.TXT|.csv', full.names = T,recursive = F)

lascarfilepath_caa <- "../Data/CAA Data/LASCAR Files"
file_list_lascar_caa <- list.files(lascarfilepath_caa, pattern='.txt|.TXT|.csv', full.names = T,recursive = F)

beaconfilepath <- "../Data/Data from the team/Beacon Logger"
file_list_beacon <- list.files(beaconfilepath, pattern='.csv|.CSV', full.names = T,recursive = T)


#Load data
predicted_ses<-readRDS("Processed Data/predicted_ses.rds")
preplacement <- as.data.table(readRDS("Processed Data/preplacement.rds"))
preplacement <- merge(predicted_ses,preplacement,by="HHID",all.y = TRUE)
postplacement <- readRDS("Processed Data/postplacement.rds")
mobenzi_indepth <- readRDS("Processed Data/mobenzi_indepth.rds")
mobenzi_rapid <- readRDS("Processed Data/mobenzi_rapid.rds")
equipment_IDs <- readRDS("Processed Data/equipment_IDs.rds")
lascar_cali_coefs <- readRDS("Processed Data/lascar_calibration_coefs.rds")
CO_calibrated_timeseries <- readRDS("Processed Data/CO_calibrated_timeseries.rds")
pats_data_timeseries <- readRDS("Processed Data/pats_data_timeseries.rds")
tsi_timeseries <- as.data.table(readRDS("Processed Data/tsi_timeseries.rds"))
beacon_logger_data<- readRDS("Processed Data/beacon_logger_data.rds")
upasmeta<- readRDS("Processed Data/upasmeta.rds")
beacon_meta_qaqc<-readRDS("Processed Data/beacon_meta_qaqc.rds")
lascar_meta<-readRDS("Processed Data/lascar_meta.rds")
tsi_meta_qaqc<-readRDS("Processed Data/tsi_meta_qaqc.rds")
pats_meta_qaqc<-readRDS("Processed Data/pats_meta_qaqc.rds")
beacon_logger_raw <- readRDS("Processed Data/Beacon_RawData.rds")

# ecm_data <- readRDS("../Data/analysis-20200421/ecm_data.RDS")
# dot_data <- readRDS("../Data/analysis-20200421/dot_data.RDS")
ecm_meta_data <- readRDS("Processed Data/ecm_meta_data.rds")
ecm_dot_data <- readRDS("../Data/analysis-20200421/ecm_dot_data.RDS") %>%
  dplyr::mutate(stove_type =
                  case_when(stove_type == 'other' ~ 'stove_type_other',
                            stove_type == 'tsf' ~ 'traditional_non_manufactured',
                            TRUE ~ as.character(stove_type)),
                HHID = pm_hhid,
                datetime = time_chunk,
                sampletype = pm_monitor_type,
                qc = "good") %>%
  # dplyr::filter(HHID != 'KE511-KE06' & HHID != 'KE508-KE12') %>%
  dplyr::select(-other_people_use,-meter_name,-meter_id,-notes,-creator_username,-pm_monitor_type,
                -unops,-stove_type_other,-mission_id,-pm_hhid,-time_chunk,-pm_monitor_id,-pm_filter_id,-campaign) %>%
  as.data.table()



# Excel metadata import
gravimetric_path <- "../Data/Data from the team/Gravimetric/UNOPS E2E_v2.xlsx"
gravimetric_data <- grav_import_fun(gravimetric_path)
meta_emissions<- readRDS("Processed Data/meta_emissions.RDS")

metadata_ambient <- ambient_import_fun(path_other,sheetname='Ambient Sampling')
ambient_data = readRDS("~/Dropbox/UNOPS emissions exposure/E2E Data Analysis/Processed Data/ambient_data.rds")



