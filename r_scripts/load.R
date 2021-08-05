if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate,plyr,dplyr,reshape2,devtools,shiny,shinydashboard,dygraphs,DT,shinyjs,tools,data.table,writexl,zoo,readxl
,gmailr,mailR,cronR,miniUI,shinyFiles,ggplot2,stringr,chron,doParallel,foreach,openxlsx,gridExtra,egg,cowplot,corrgram,
factoextra,scales,htmlwidgets,tidyfast,tidyr,kableExtra,janitor)

registerDoParallel(cores=detectCores()-2)

