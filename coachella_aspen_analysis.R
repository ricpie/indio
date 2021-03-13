rm(list = ls()) #clear the environment
graphics.off()
## Analyze Coachella ASPEN data
# Import data, perform QAQC check.  If email == 1, send an email out.
#Set directory with this file as the working directory.
setwd("~/Dropbox/Coachella Source Apportionment PHI/Analysis")
dir.create("tester",showWarnings = F)
dir.create("odk data",showWarnings = F)  #Put odk data downloads into here.
dir.create("plots",showWarnings = F)
dir.create("processed data",showWarnings = F)
dir.create("QA Reports",showWarnings = F)
dir.create("results",showWarnings = F)


#Keys: instrument_id, location, size

source('r_scripts/load.R')
source('r_scripts/ASPEN_functions.R')
email = 1 #Set to 1 to send out summary qaqc email, else 0


#Import Excel meta data (instrument start/stop times, filter IDs, etc.)
field_log = readxl::read_xlsx(list.files(path = "~/Dropbox/Coachella Field/Data",pattern = 'AspenData', recursive = T, full.names = T),
                              skip = 2,col_names  = TRUE,sheet = 'Sample Form Data') %>%
  dplyr::mutate(across(contains("date") | contains("time"), as.character)) %>%
  dplyr::mutate(across(contains("time"), str_remove, "1899-12-31")) %>%
  dplyr::mutate(
    datetime_setup = as.POSIXct(paste(Setup_Date, Setup_Time),
                                tz="America/Los_Angeles"),
    datetime_start = case_when(!is.na(`Programmed Sample Start Date`) ~
                                 as.POSIXct(paste(`Programmed Sample Start Date`,
                                                  `Programmed Sample Start Time`),
                                            format="%Y-%m-%d",
                                            tz="America/Los_Angeles")),
    datetime_end   = case_when(!is.na(`Programmed Sample End Date`) ~
                                 as.POSIXct(paste(`Programmed Sample End Date`,
                                                  `Programmed Sample End Time`),
                                            format="%Y-%m-%d",
                                            tz="America/Los_Angeles"))) %>% 
  dplyr::select(-"Programmed Sample End Date",-"Programmed Sample End Time",-"Programmed Sample Start Date",
                -"Programmed Sample Start Time") %>% 
  tidyr::pivot_longer(cols = c("PM2.5 Quartz Filter ID","PM10 Quartz Filter ID",
                               "PM2.5 Teflon Filter ID","PM10 Teflon Filter ID"),
                      names_to = "Type",
                      values_to="Filter_ID") %>% 
  dplyr::mutate( Filter_Type = ifelse(Type %like% "Quartz","Quartz","Teflon")) %>% 
  dplyr::rename_all(function(x) paste0("excel_", x)) %>% 
  dplyr::mutate(CartridgeID = toupper(excel_Filter_ID),
                excel_Comments = toupper(excel_Comments))


#Import list of filters assembled in Colorado
filter_list = readxl::read_xlsx(list.files(path = "~/Dropbox/Coachella Field/Data",
                                           pattern = 'AspenData', recursive = T, full.names = T),
                                skip = 0,sheet = 'Filter List')[,2:5] 

colnames(filter_list) <-c("FilterID", "FilterType","CartridgeID","DateSent")
filter_list <- filter_list %>% 
  dplyr::filter(!is.na(DateSent)) %>% 
  dplyr::mutate(DateSent = as.Date(DateSent),
                FilterID = toupper(FilterID),
                CartridgeID = toupper(CartridgeID),
                FilterID = case_when(FilterType == "Quartz" ~ paste0(as.character(DateSent),"_",CartridgeID),
                                     TRUE ~ FilterID))


#Import ASPEN data
aspen_files = list.files(path = "~/Dropbox/Coachella Field/Data/Field Data Collected",pattern = 'ASPEN00', recursive = T, full.names = T)
aspen_data = rbindlist(lapply(aspen_files,read_aspen)) 

aspen_files[sapply(aspen_files, file.size) > 10000]


aspen_header <- rbindlist(lapply(aspen_files,read_aspen_header)) %>% 
  as.data.frame() %>% 
  dplyr::rename(Filter1ID = CIDfilter1,
                Filter2ID = CIDfilter2) %>% 
  dplyr::select(ASPENserial,LogFilename,Filter1ID,Filter2ID,Filter1StartDateTimeUTC,Filter2StartDateTimeUTC,EndDateTimeUTC,
                Filter1ShutdownMode,Filter2ShutdownMode,Filter1SampledRuntime,Filter2SampledRuntime,Filter1AverageVolumetricFlowRate,
                Filter2AverageVolumetricFlowRate,Filter1SampledVolume,Filter2SampledVolume) 

aspen_header <- rbind(aspen_header %>% 
                        dplyr::select(ASPENserial,LogFilename,starts_with("Filter1")) %>% 
                        dplyr::mutate(Filter = "Filter1") %>% 
                        dplyr::rename_all(function(x) gsub("Filter1","", x)),
                      aspen_header %>% 
                        dplyr::select(ASPENserial,LogFilename,starts_with("Filter2")) %>% 
                        dplyr::mutate(Filter = "Filter2") %>% 
                        dplyr::rename_all(function(x) gsub("Filter2","", x))) %>% 
  dplyr::rename_all(function(x) paste0("file_", x)) %>% 
  dplyr::rename(CartridgeID = file_ID) %>% 
  dplyr::arrange(file_LogFilename,file_StartDateTimeUTC) %>% 
  dplyr::mutate(file_start_date = as.Date(file_StartDateTimeUTC),
                CartridgeID = toupper(CartridgeID),
                SizeCut = case_when(file_Filter %like% "Filter1" ~ "PM2.5",
                                    file_Filter %like% "Filter2" ~ "PM10",
                                    TRUE ~ "NA")) %>% 
  dplyr::mutate(StartDateTimeLocal = with_tz(file_StartDateTimeUTC, 
                                             tzone="America/Los_Angeles"),
                date_start = date(StartDateTimeLocal)) %>%
  dplyr::filter(date(StartDateTimeLocal) %in% date(field_log$excel_datetime_start) | 
                  hour(StartDateTimeLocal) == 0)

#What do I want to do?
#1. Check the filter IDs.  Are they consistent between the log sheet and the instrument?
#2. Check the sample operation - sample duration, flow rate, any error messages.  Done.
#3.  Send this info to Madeleine and Christian and Edgar. Done
#4. Send reminder on how when to ship filters.

#1 Merge file with meta data.  And then with filter List to make sure it exists therein

aspen_header_merged_temp = dplyr::full_join(aspen_header,field_log,by = c("CartridgeID")) %>% 
  dplyr::mutate(volume_flag = case_when(!file_AverageVolumetricFlowRate %between% c(1.8,2.2) ~ 1,
                                        !file_AverageVolumetricFlowRate %between% c(1.8,2.2) ~ 1,
                                        is.na(file_AverageVolumetricFlowRate) | 
                                          is.na(file_AverageVolumetricFlowRate) ~ 0 ),
                meta_filter_merge_flag = ifelse(is.na(excel_Location),1,0),
                duration_flag = ifelse(!file_SampledRuntime %between% c(43.2,52.8),1,0)) %>% 
  dplyr::select(CartridgeID,file_ASPENserial,file_LogFilename,
                file_StartDateTimeUTC,file_start_date,file_ShutdownMode,file_SampledRuntime,
                file_AverageVolumetricFlowRate,file_SampledVolume,excel_datetime_start,
                excel_Comments, excel_Filter_Type,volume_flag,
                meta_filter_merge_flag,duration_flag,SizeCut
                # `excel_Flow measured in 22 PM2.5`,`excel_Flow measured in 22 PM10`,
                # `excel_Flow measured in 27 PM10`,`excel_Flow measured in 22 PM2.5`,`excel_Inlet pieces cleaned with isopropyl alcohol?`,
             ) 

aspen_header_merged1 <- dplyr::left_join(aspen_header_merged_temp,filter_list, by="CartridgeID") %>% 
  dplyr::mutate(excel_sent_datediff = as.Date(DateSent)-as.Date(excel_datetime_start), #This should be negative for all.  And then keep the nearest or NA
                sampletype = case_when(excel_Comments %like% "BLANK" ~ "blank",
                                       TRUE ~ "ambient")) %>%
  dplyr::filter(is.na(file_start_date) | #Keep NA file start dates since some files were not saved.
                  (file_SampledRuntime>2 | sampletype %like% "blank"))  %>% #Runs less than 1 hour are from tests.
  dplyr::mutate(excel_file_datediff = as.Date(excel_datetime_start) - as.Date(file_start_date)) %>% #Should be negative, and nearest!
  dplyr::arrange(CartridgeID,desc(sampletype),file_start_date,excel_file_datediff) %>% 
  dplyr::mutate(R_notes = case_when(excel_sent_datediff>0 ~ "Ensure that the Excel file is up to date, and not merging with another row on Cartridge ID incorrectly.",
                                    excel_file_datediff>0 ~ "ASPEN file may be missing, impute data as possible"))  %>%
  select(FilterID, everything())


aspen_header_merged <- aspen_header_merged1 %>% 
  dplyr::filter(excel_sent_datediff < 0,
                (is.na(excel_file_datediff) | sampletype %like% "blank" | abs(excel_file_datediff) < 2))  %>% #Set to 2 to give some leeway in bad data entry.
  dplyr::group_by(FilterID) %>% 
  dplyr::arrange(desc(excel_sent_datediff)) %>% 
  dplyr::mutate(row_number = row_number()) %>% 
  dplyr::filter(row_number == 1)
  
# dplyr::filter(1 == row_number())

#These are the rows that get filtered out here.  Should be due to duplicate cartridge IDs for filters that have not yet been sent out- but check anyway!
setdiff(aspen_header_merged1$FilterID,aspen_header_merged$FilterID) 


filename = paste0("~/Dropbox/Coachella Field/Data/QA Reports/Indio_QA_Report_",  Sys.Date(), ".xlsx")
write.xlsx(list(complete_list = aspen_header_merged1,clean_list = aspen_header_merged),file = filename)


