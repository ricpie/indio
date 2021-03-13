# Written by:    Jessica Tryner
# Date written:  August 30, 2020
# Last modified: October 23, 2020
# Current ASPEN Box firmware version: 99

# Purpose: This function reads just the header data from an ASPEN Box log file.  
 
# Inputs: 
# (1) file: The file name
# (2) long_format: A logical indicating whether the resulting data frame should be split into two
#                  rows containing data for each pump (filter 1 and filter 2)

# Output: If long_format = FALSE, a data frame with one row and 42 columns containing the data in the ASPEN Box sample file header. 
#         If long_format = TRUE, a data frame with three rows and 32 columns containing the data in the ASPEN Box sample file header.

read_aspen_header <- function(file, long_format=F){
  
  require(tidyr)
  require(dplyr)
  
  header_lines <- readLines(file, n=73, skipNul=T)
  
  header_lines <- header_lines[header_lines != ""]
  
  n_max    <- grep("SAMPLE LOG", header_lines) - 1
  
  df <- read.csv(file, nrows=n_max, stringsAsFactors=F) %>%
        dplyr::select(PARAMETER, VALUE) %>%
        tidyr::pivot_wider(names_from="PARAMETER", values_from="VALUE") %>%
        dplyr::select(-"SAMPLE IDENTIFICATION", -"Calibration Coefficients", -"SETUP SUMMARY", -"SAMPLE SUMMARY", -"SAMPLE LOG") 
  
  if("_CO2_slope" %in% colnames(df)){
    df <- df %>% dplyr::rename(CO2Slope     = `_CO2_slope`,
                               CO2Intercept = `_CO2_intercept`)
  }
  
  df <- df %>%
        dplyr::mutate_at(c("COserial","NO2serial","O3serial","CO2Slope","CO2Intercept",
                           "GPSUTCOffset","ProgrammedStartUnixtime",
                           "ProgrammedRuntime", "Filter1ProgrammedRuntime","Filter2ProgrammedRuntime",
                           "Filter1VolumetricFlowRate","Filter2VolumetricFlowRate","Filter1DutyCycle","Filter2DutyCycle",
                           "Filter1ShutdownMode","Filter2ShutdownMode","Filter1SampledVolume","Filter2SampledVolume",
                           "ASPENSampledRuntime","Filter1SampledRuntime","Filter2SampledRuntime",
                           "Filter1AverageVolumetricFlowRate","Filter2AverageVolumetricFlowRate"), as.numeric) %>%
        dplyr::mutate_at(c("StartDateTimeUTC","Filter1StartDateTimeUTC","Filter2StartDateTimeUTC",
                           "Filter1LastUpdateUTC","Filter2LastUpdateUTC"),
                         as.POSIXct, format="%Y-%m-%dT%H:%M:%S", tz="UTC") %>%
        dplyr::mutate(LogFilename = gsub("/sd/", "", LogFilename),
                      Firmware    = substr(Firmware, 1, regexpr(" ", Firmware)[1]-1))
  
  if(long_format){
    
    df1 <- df %>%
      dplyr::select(-contains("Filter2"), -HP2serial) %>%
      dplyr::mutate(Pump = "Filter1")
    
    df2 <- df %>%
      dplyr::select(-contains("Filter1"), -HP1serial) %>%
      dplyr::mutate(Pump = "Filter2")
    
    df_names <- gsub("Filter2", "Pump", colnames(df2))
    df_names <- gsub("filter2", "", df_names)
    
    colnames(df1) <- df_names
    colnames(df2) <- df_names
    
    df <- dplyr::bind_rows(df1, df2)
  }
  
  return(df)
}

# Written by:    Jessica Tryner
# Date written:  August 30, 2020
# Last modified: October 21, 2020
# Current ASPEN Box firmware version: 99

# Purpose: This function reads the log data from an ASPEN Box log file and appends some data from the file header
#          for easy sample identification.  

# Inputs: (1) file: The file name

# Output: A data frame with 56 columns. 

read_aspen <- function(file){
  
  require(dplyr)
  
  # Get header data
  df_h <- read_aspen_header(file) %>% 
          dplyr::select(any_of(c("LogFilename","ASPENserial","SampleName","HGserial","COserial","NO2serial","O3serial","CO2serial")))
  
  # Read 73 lines, find the line with the SAMPLE LOG header, the add 1 to get the number of lines to skip
  header_lines <- readLines(file, n=73)
  nskip        <- match("SAMPLE LOG", header_lines) + 2
  
  # Read the log data
  df <- read.csv(file, header=T, skip=nskip, stringsAsFactors=F)
  
  if(nrow(df) > 0){
    
    df <- cbind(df_h, df) %>%
          dplyr::mutate(SampleTime = ifelse(SampleTime == "99:99:99", NA, SampleTime),
                        SampleTime = strsplit(SampleTime,":"),
                        SampleTime = as.difftime(3600*as.numeric(sapply(SampleTime, `[`, 1)) +
                                                   60*as.numeric(sapply(SampleTime, `[`, 2)) +
                                                   as.numeric(sapply(SampleTime, `[`, 3)), units="secs"),
                        DateTimeUTC = as.POSIXct(DateTimeUTC, format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
  }
  
  return(df)
}