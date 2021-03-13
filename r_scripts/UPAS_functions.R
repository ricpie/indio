# Written by:    Jessica Tryner
# Last modified: October 21, 2020

# Purpose: This function reads just the header data from a UPAS log file.  
# If the user just wants to calculate the time-averaged PM concentration measured using the filter sample, 
# all of the sample summary information that is needed to do so is stored in the header and read by this function.
# This function can be used in conjuction with lapply() or map() to read in sample summary data from any number of log files
# and create a data frame that contains a line with the summary information for each sample. 

# Inputs: 
# (1) The file name
# (2) update_names: a logical variable indicating whether variable names in log files recorded using firmware version 100
#                   should be updated to match variable names in log files recorded using firmware versions > 100 
#                   (default = FALSE). If TRUE, "CumulativeSamplingTime" will be updated to "LifetimeSampleRuntime",
#                   "StartDateTime" will be updated to "StartDateTimeUTC", and "AverageVolumetricFlow" will be updated to
#                   "AverageVolumetricFlowRate".

# Output: A data frame with one row and 28 to 34 columns (depending on the firmware version) containing the data in the UPAS sample file header. 

read_upas_header <- function(file, update_names=FALSE){
  
  require(dplyr)
  
  header_lines <- readLines(file, n=103)
  
  n_max    <- grep("AverageVolumetricFlow", header_lines)
  n_cal    <- grep("CALIBRATION COEFFICIENTS", header_lines) 
  n_cal    <- ifelse(length(n_cal) == 0, n_max, n_cal)
  n_setup  <- grep("SETUP SUMMARY", header_lines)
  
  header_lines <- unique(c(header_lines[1:n_cal], header_lines[n_setup:n_max]))
  header_lines <- header_lines[!is.na(header_lines)]
  header_lines <- header_lines[!(header_lines %in% c("",
                                                     "PARAMETER,VALUE,UNITS/NOTES",
                                                     "SAMPLE IDENTIFICATION",
                                                     "SETUP SUMMARY",
                                                     "SAMPLE SUMMARY",
                                                     "CALIBRATION COEFFICIENTS"))]
  
  df <- sapply(strsplit(header_lines,","), `[`, 2) %>%
        t() %>%
        data.frame(stringsAsFactors=F)
  
  colnames(df) <- sapply(strsplit(header_lines,","), `[`, 1)
  
  df <- df %>%
        dplyr::mutate_at(c("UPASserial","GPSUTCOffset","StartOnNextPowerUp","ProgrammedStartDelay","ProgrammedRuntime",
                           "VolumetricFlowRate","DutyCycle","DutyCycleWindow",
                           "GPSEnabled","LogFileMode","LogInterval","AppLock",
                           "StartBatteryCharge","StartBatteryVoltage","EndBatteryCharge","EndBatteryVoltage",
                           "ShutdownMode","SampledVolume","SampledRuntime","LoggedRuntime"),
                         as.numeric) %>%
        dplyr::mutate_at(c("StartOnNextPowerUp", "GPSEnabled"), as.logical) %>%
        dplyr::mutate(UPASfirmware    = sapply(strsplit(UPASfirmware,"-"), `[`, 2),
                      UPASfirmware    = as.numeric(gsub("rev", "", UPASfirmware)),
                      UPASlogFilename = gsub("/sd/", "", UPASlogFilename),
                      LogFileMode     = ifelse(LogFileMode == 0, "normal", "debug"),
                      ShutdownReason  = case_when(ShutdownMode == 0 ~ "unknown error",
                                                  ShutdownMode == 1 ~ "user pushbutton stop",
                                                  ShutdownMode == 2 ~ "depleted battery",
                                                  ShutdownMode == 3 ~ "completed preset sample duration",
                                                  ShutdownMode == 4 ~ "thermal protection",
                                                  ShutdownMode == 5 ~ "max power at initialization",
                                                  ShutdownMode == 6 ~ "max power during sample",
                                                  ShutdownMode == 7 ~ "blocked flow")) %>%
        dplyr::select(1:match("ShutdownMode",colnames(df)), ShutdownReason, (match("ShutdownMode",colnames(df))+1):ncol(df))
  
  if(df$UPASfirmware == 100){
    
    df <- df %>%
          dplyr::mutate_at(c("PowerCycles","CumulativeSamplingTime","AverageVolumetricFlow"), as.numeric) %>%
          dplyr::mutate(StartDateTime = as.POSIXct(StartDateTime, format="%Y-%m-%dT%H:%M:%SUTC", tz="UTC")) 
        
    if(update_names){
      
      df <- df %>% dplyr::rename(LifetimeSampleRuntime     = CumulativeSamplingTime,
                                 StartDateTimeUTC          = StartDateTime,
                                 AverageVolumetricFlowRate = AverageVolumetricFlow)}
  }else{
    
    df <- df %>%
          dplyr::mutate_at(c("LifetimeSampleCount","LifetimeSampleRuntime","FlowOffset","AverageVolumetricFlowRate"), as.numeric) %>%
          dplyr::mutate_at(c("StartDateTimeUTC", "EndDateTimeUTC"), as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
          dplyr::mutate(SampleName  = gsub("_+$", "", SampleName),
                        SampleName  = ifelse(SampleName != "", SampleName, NA),
                        CartridgeID = gsub("_+$", "", CartridgeID),
                        CartridgeID = gsub("-+$", "", CartridgeID),
                        CartridgeID = ifelse(CartridgeID != "", CartridgeID, NA))}
  
  return(df)
}

# Written by:    Jessica Tryner
# Last modified: October 21, 2020

# Purpose: This function reads the real-time log data from a UPAS log file.  
# It also reads the header data and adds key variables needed to identify the sample to the data frame with the log data. 
# This function can be used in conjuction with lapply() or map() to read in log data from any number of files and combine 
# them into a single data frame.

# Inputs: 
# (1) The file name
# (2) update_names: a logical variable indicating whether variable names in log files recorded using firmware version 100
#                   should be updated to match variable names in log files recorded using firmware versions > 100 
#                   (default = FALSE). If TRUE, "UTCDateTime" will be updated to "DateTimeUTC", "VolFlow" will be updated
#                   to "VolumetricFlowRate", and "StartDateTime" will be updated to "StartDateTimeUTC". For diagnostic 
#                   log files, "gpsspeed" will be updated to "GPSspeed" and "gpsquality" will be updated to "GPSquality". 

# Output: A data frame containing 29 to 45 columns, depending on the UPAS firmware version and whether the file is a "normal" 
#         log file or a "debug" log file.   

read_upas <- function(file, update_names=FALSE){
  
  require(dplyr)
  
  # Get header data
  df_h <- read_upas_header(file, update_names=update_names) %>%
          dplyr::select(any_of(c("UPASlogFilename","UPASserial","UPASfirmware","SampleName","CartridgeID", 
                                 "StartDateTimeUTC","StartDateTime","LogFileMode")))
  
  # Read 103 lines, find the line with the SAMPLE LOG header, the add 1 to get the number of lines to skip
  header_lines <- readLines(file, n=103)
  nskip        <- match("SAMPLE LOG", header_lines) + 2
  
  # Read the log data
  df <- read.csv(file, header=T, skip=nskip, stringsAsFactors=F)
  
  if(nrow(df) > 0){
    
    df <- cbind(df_h, df) %>%
          dplyr::mutate(SampleTime = ifelse(SampleTime == "99:99:99", NA, SampleTime),
                        SampleTime = ifelse(!is.na(SampleTime), strsplit(SampleTime,":"), SampleTime),
                        SampleTime = as.difftime(3600*as.numeric(sapply(SampleTime, `[`, 1)) + 
                                                   60*as.numeric(sapply(SampleTime, `[`, 2)) + 
                                                   as.numeric(sapply(SampleTime, `[`, 3)), units="secs"))
  
    if("UTCDateTime" %in% colnames(df)){ # For firmware version 100
      
      df <- df %>% dplyr::mutate(UTCDateTime = as.POSIXct(UTCDateTime, format="%Y-%m-%dT%H:%M:%S", tz="UTC")) 
      
      if(update_names){
        df <- df %>% dplyr::rename(DateTimeUTC        = UTCDateTime,
                                   VolumetricFlowRate = VolFlow)}
      
    }else{ # For firmware version > 100
      df <- df %>% dplyr::mutate(DateTimeUTC = as.POSIXct(DateTimeUTC, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
    }
    
    if(!is.null(df_h$LogFileMode)){
      if((df_h$LogFileMode == "debug") & ("PumpsON" %in% colnames(df))){ # For debug files
        
        df <- dplyr::mutate_at(df, c("PumpsON","Dead","BCS1","BCS2","BC_NPG"), as.logical)
        
        if(("gpsspeed" %in% colnames(df)) & update_names){
          df <- df %>% dplyr::rename(GPSspeed   = gpsspeed,
                                     GPSquality = gpsquality)}
      }
    }
  }
  
  return(df)
}

# Purpose: This function reads the calibration constants and diagostic test results from a diagnostic log file. 
# Note that the 'read_header' function will read the normal header data from a diagnostic log file with no issues.
# Similarly, the 'read_upas' function will read the real-time log data from a diagnostic log file with no issues.
# This function is specifically designed to return the calibration constants and diagnostic test results, 
# which are not included in a normal UPAS log file.  

# Inputs: 
# (1) The file name
# (2) update_names: a logical variable indicating whether variable names in log files recorded using firmware version 100
#                   should be updated to match variable names in log files recorded using firmware versions > 100 
#                   (default = TRUE). If TRUE, "StartDateTime" will be updated to "StartDateTimeUTC" and
#                   "AverageVolumetricFlow" will be updated to "AverageVolumetricFlowRate".

# Output: A data frame with four rows and 33 columns containing:
#         (a) the calibration constants for the UPAS and 
#         (b) the results of the diagnostic test. 

read_diag <- function(file, update_names=TRUE){
  
  require(dplyr)
  
  # Read in the standard UPAS header data
  df_h <- read_upas_header(file, update_names=update_names) %>%
          dplyr::select(any_of(c("UPASserial","UPASfirmware","UPASlogFilename","SampleName","CartridgeID", "LogFileMode",
                                 "StartDateTimeUTC","StartDateTime","StartDateTimeLocal","StartBatteryCharge","StartBatteryVoltage",
                                 "EndDateTimeUTC","EndDateTimeLocal","EndBatteryCharge","EndBatteryVoltage",
                                 "ShutdownMode","SampledVolume","SampledRuntime","LoggedRuntime",
                                 "AverageVolumetricFlowRate","AverageVolumetricFlow")))
  
  if(df_h$LogFileMode != "debug"){

    warning(sprintf("Warning: %s is not a debug file. NULL returned.", file))
    return()
    
  }else{
    
    # Read a maximum of 103 header lines
    header_lines <- readLines(file, n=103)
    
    n_cal   <- grep("CALIBRATION COEFFICIENTS", header_lines) 
    n_setup <- grep("SETUP SUMMARY", header_lines)
    n_diag  <- grep("DIAGNOSTIC TEST", header_lines)
    n_log   <- grep("SAMPLE LOG", header_lines)
    
    # Get calibration coefficients
    if(n_cal > n_setup){
      cal <- header_lines[(n_cal+1):(n_diag-1)]
    }else{
      cal <- header_lines[(n_cal+1):(n_setup-1)]
    }
    
    cal <- cal[cal != ""]
    
    df_cal <- sapply(strsplit(cal,","), `[`, 2) %>%
              t() %>%
              data.frame(stringsAsFactors=F)
    
    colnames(df_cal) <- sapply(strsplit(cal,","), `[`, 1)
    
    df_cal <- df_cal %>%
              mutate_at(colnames(df_cal)[which(colnames(df_cal) != "CalDateTime")], as.numeric) %>%
              dplyr::mutate(CalDateTime = as.POSIXct(CalDateTime, tz="UTC",
                                                     tryFormats=c("%Y-%m-%dT%H:%M:%S",
                                                                  "%d-%m-%y_%H:%M:%S")))
    
    # Get diagnostic test results
    diag <- header_lines[(n_diag+1):(n_log-1)]
    diag <- diag[diag != ""]
    
    names_diag <- diag[grepl("MFSVolt", diag)] %>% unique() %>% strsplit(",") %>% unlist()
    
    if(length(grep("VolumetricFlowRate", names_diag)) == 0){
      names_diag <- gsub("VolFlow","VolumetricFlowRate", names_diag)
    }
    
    df_diag <- strsplit(diag, ",") %>%
               lapply(t) %>%
               lapply(data.frame, stringsAsFactors=F) %>%
               dplyr::bind_rows()
    
    colnames(df_diag) <- c("condition", names_diag)
    
    df_cond <- data.frame(condition=df_diag$condition[!is.na(df_diag$condition)], stringsAsFactors=F) 
    
    df_diag <- df_diag %>%
               dplyr::select(-condition) %>%
               dplyr::filter(!is.na(PumpP),
                             !(PumpP %in% c("(hPa)","PumpP"))) %>%
               dplyr::mutate_all(as.numeric)
    
    # Combine header data, calibration coefficients, and diagnostic test results
    df <- cbind(df_h, df_cal, df_cond, df_diag)
    
    return(df)
  }
}