rm(list = ls()) #clear the environment
# Import data downloaded from ARB websites and plot up


#### Import data
dir <- '~/Dropbox/Coachella Source Apportionment PHI/Analysis'
setwd(dir)
source('r_scripts/load.R')
`%notlike%` <- Negate(`%like%`)

files_ambient = list.files('~/Dropbox/Coachella Source Apportionment PHI/Ambient Data',
                           pattern = '.csv',
                           full.names = TRUE)

ambdata = data.table::rbindlist(lapply(files_ambient,function(x) utils::read.csv(x,header=TRUE)),fill=TRUE) 

ambdata <- ambdata %>%
  dplyr::filter(nchar(as.character(name)) > 1) %>%
  dplyr::mutate(name = gsub(" ","",name),
                date = as.Date(date),
                year = format(as.Date(date),"%Y"),
                month = as.factor(month(date)),
                year_month = paste0(year,"-",month),
                units = as.character(units),
                ws = ws*.447) %>% #Convert to m/2
  dplyr::mutate(value = case_when(variable %like% 'OZONE' ~ value * 1000,
                                  TRUE ~ value),
                units = case_when(variable %like% 'OZONE' ~ "ppb",
                                  variable %like% 'WINSPD' ~ "m/s",
                                  TRUE ~ units)) %>%
  dplyr::distinct()




####PM10 Analysis

pm10_timeseries_plot <-  ambdata %>% 
  dplyr::filter(variable %like% 'PM10_LHR')  %>% 
  ggplot(aes(y = value, x = date, color = variable)) +
  geom_point(alpha = 0.3) + theme_minimal() +  
  geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  facet_wrap(~name, scales = "free", ncol = 1) 

pm10_boxplot <- ambdata %>% 
  dplyr::filter(variable %like% 'PM10_LHR')  %>% 
  ggplot(aes(y = value, x = year_month,color = variable)) +
  geom_boxplot(alpha = 0.3) + theme_minimal() +  
  # geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  coord_cartesian(ylim = c(0,200)) +
  facet_wrap(~name, scales = "free", ncol = 1) 
ggsave('plots/pm10_boxplot.png',last_plot())

pm10_teom_boxplot <- ambdata %>% 
  dplyr::filter(variable %like% 'PMTEOM')  %>% 
  ggplot(aes(y = value, x = year_month,color = variable)) +
  geom_boxplot(alpha = 0.3) + theme_minimal() +  
  # geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  coord_cartesian(ylim = c(0,200)) +
  facet_wrap(~name, scales = "free", ncol = 2) 
ggsave('plots/pm10_teom_boxplot.png',last_plot())

####PM2.5 Analysis
pm25_timeseries_plot <-  ambdata %>% 
  dplyr::filter(variable %like% 'PM25HR')  %>% 
  ggplot(aes(y = value, x = date, color = variable)) +
  geom_point(alpha = 0.3) + theme_minimal() +  
  geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  facet_wrap(~name, scales = "free", ncol = 1) 

pm25_hrly_boxplot <- ambdata %>% 
  dplyr::filter(variable %like% 'PM25HR')  %>% 
  ggplot(aes(y = value, x = year_month,color = variable)) +
  geom_boxplot(alpha = 0.3) + theme_minimal() +  
  # geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  coord_cartesian(ylim = c(0,100)) +
  facet_wrap(~name, scales = "free", ncol = 1) 
ggsave('plots/pm25_hrly_boxplot.png',last_plot())

pm25_daily_boxplot <- ambdata %>% 
  dplyr::filter(variable == 'PM25')  %>% 
  ggplot(aes(y = value, x = year_month,color = variable)) +
  geom_boxplot(alpha = 0.3) + theme_minimal() +  
  # geom_smooth(method = "lm", formula = 'y ~ x',color = 'black') +
  coord_cartesian(ylim = c(0,100)) +
  facet_wrap(~name, scales = "free", ncol = 2) 
ggsave('plots/pm25_daily_boxplot.png',last_plot())





####Wind Analysis

#Join wind data with corresponding pollution data to allow us to do pollution roses.
wideamb <- 
  dplyr::left_join(ambdata %>% dplyr::select(-ws,-wd),
                   ambdata %>% 
                     dplyr::filter(variable %like% 'WINSPD') %>%
                     dplyr::select(date,start_hour,ws,wd,name),
                   by=c('date','start_hour','name'))

windIndio <- ambdata %>% 
  dplyr::filter(variable %like% 'WINSPD') %>%
  dplyr::filter(name %like% 'Indio-JacksonStreet')
windroseIndio <- windRose(windIndio,ws="ws",wd="wd",type = "season",main='Indio-JacksonStreet')
ggsave('plots/WindRoseIndio.png',last_plot())

windMecca <- ambdata %>% 
  dplyr::filter(variable %like% 'WINSPD') %>%
  dplyr::filter(name %like% 'Mecca-65705JohnsonStreet')
windroseMecca <- windRose(windMecca,ws="ws",wd="wd",type = "season",main='Mecca-65705JohnsonStreet',statistic = "prop.count")
ggsave('plots/WindRoseMecca.png',last_plot())

windroseDifferenceMeccaIndio <-windRose(dplyr::left_join(windMecca,windIndio,by = c('date','start_hour')),
                                        ws="ws.x",wd="wd.x",ws2 = "ws.y",wd2 = "wd.y",type = "season",main='Mecca-Indio Differences',
                                        statistic = "prop.count")
ggsave('plots/WindDifferenceMeccaIndio.png',last_plot())



#### Pollution roses

wider <- pivot_wider(wideamb,names_from = variable, values_from = value)

RoseTEOMIndio <- pollutionRose(wider %>% 
                                 dplyr::filter(name %like% 'Indio-JacksonStreet'),
                               pollutant = "PMTEOM",type = "season",main='Indio-Jackson Street PM2.5 TEOM')
ggsave('plots/RoseTEOMIndio.png',last_plot())

roseOzoneIndio <- pollutionRose(wider %>% 
                                  dplyr::filter(name %like% 'Indio-JacksonStreet'),
                                pollutant = "OZONE",type = "season",main='Indio-Jackson Street Ozone')
ggsave('plots/RoseOzoneIndio.png',last_plot())


roseTEOMMecca <- pollutionRose(wider %>% 
                                 dplyr::filter(name %like% 'Mecca-65705JohnsonStreet'),
                               pollutant = "PMTEOM",type = "season",main='Mecca-65705 Johnson Street PM2.5 TEOM')
ggsave('plots/RoseTeomMecca.png',last_plot())


#### Windspeed effect on pm
windPM10Mecca <- wider %>% 
  dplyr::filter(name %like% 'Mecca-65705JohnsonStreet') %>%
  ggplot(aes(x = ws,y=PMTEOM)) +
  geom_point(alpha = 0.3) + theme_minimal() +  
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(0,100)) +
  xlab('Wind speed (m/s)') + 
  ylab('PM10 µg/m-3')+
  ggtitle('Mecca Johnson Street')
ggsave('plots/windspeedPM10Indio.png',last_plot())

windPM10Indio <- wider %>% 
  dplyr::filter(name %like% 'Indio-JacksonStreet') %>%
  ggplot(aes(x = ws,y=PMTEOM)) +
  geom_point(alpha = 0.3) + theme_minimal() +  
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(0,100)) +
  xlab('Wind speed (m/s)') + 
  ylab('PM10 µg/m-3') +
  ggtitle('Indio Jackson Street')
ggsave('plots/windspeedPM10Indio.png',last_plot())


windPM25Bombay <-  wideamb %>%
  dplyr::filter(name %like% 'BombayBeach',
                variable %like% 'PM25HR')  %>%
  dplyr::select(-ws,-wd) %>%
  dplyr::left_join(windMecca %>% 
                     dplyr::select(date,start_hour,ws,wd),
                   by=c('date','start_hour')) %>% 
  ggplot(aes(x = ws,y=value)) +
  geom_point(alpha = 0.3) + theme_minimal() +  
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(0,100)) +
  xlab('Wind speed (m/s)') + 
  ylab('PM2.5 µg/m-3') +
  ggtitle('Bombay PM2.5')
ggsave('plots/windspeedPM25Bombay.png',last_plot())



