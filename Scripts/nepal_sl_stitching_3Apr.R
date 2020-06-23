#******************************************************************************************#
# This is the script for stitching data from all Nepal SL together from July 2019 onwards  #
# Author: K Bhargava                                                                       #
# Last updated on: 16th June 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros
AREA <- 1.968
EFF <- 0.152
output_directory <- "Data"
#******************************************************************************************#

#*************************************************************************************************************#
# Set working directory to read and list all stitched files - file structure varies owing to diff source
filepath <- "Data/Full"
file_list <- list.files(here(filepath))
# Extract System overview Battery Power, PV-DC-coupled; Solar Charger PV current, PV voltage, Battery current, 
# Battery voltage, Battery watts, Load current; Battery Monitor State of charge
sl_nepal <- map_dfr(file_list, ~ read.csv(here(filepath, .x), header=TRUE, stringsAsFactors = FALSE) %>%
                       mutate(streetlight = gsub("_.*","",.x)))
headers <- colnames(sl_nepal)
sl_nepal <- sl_nepal[, c(headers[1], "streetlight",
                           headers[which(grepl("System.overview.PV...DC.coupled.W", headers, fixed=TRUE) | 
                                           grepl("System.overview.Battery.Power.W", headers, fixed=TRUE) | 
                                           grepl("Solar.Charger.PV.current", headers, fixed=TRUE) |
                                           grepl("Solar.Charger.PV.voltage", headers, fixed=TRUE) |
                                           grepl("Solar.Charger.Battery.current.A", headers, fixed=TRUE) |
                                           grepl("Solar.Charger.Battery.voltage.V", headers, fixed=TRUE) |
                                           grepl("Solar.Charger.Battery.watts.W", headers, fixed=TRUE) |
                                           grepl("Solar.Charger.Load.current.A", headers, fixed=TRUE) |
                                           grepl("Battery.Monitor.State.of.charge..", headers, fixed=TRUE))])] 
# Merge battery watts with battery current * voltage where missing values are found and compute PV power
sl_nepal <- sl_nepal %>% mutate(timestamp=as.POSIXct(timestamp, tz="GMT", origin="1970-01-01"),
                                Solar.Charger.PV.power.W=Solar.Charger.PV.current*Solar.Charger.PV.voltage,
                                Solar.Charger.Battery.watts.W=ifelse(is.na(Solar.Charger.Battery.watts.W),
                                    Solar.Charger.Battery.current.A*Solar.Charger.Battery.voltage.V,
                                    Solar.Charger.Battery.watts.W))
# Remove PV current, PV voltage, Battery current and Battery voltage
sl_nepal <- sl_nepal[,-c(4,5,8,9)]
write.csv(sl_nepal, file=here(output_directory,"sl_all_raw_jul_oct.csv"), row.names=FALSE)
#*************************************************************************************************************#

#*************************************************************************************************************#
# Set working directory to read all files from 30th Nov 2019 to 30th April 2020
filepath <- "Data/Raw_data"
file_list <- list.files(here(filepath))
sl_nepal2 <- data.frame()
for(i in seq_along(file_list)) {
  # SD card files have SD in the name and are xlsx format with 2 line header
  # Victron files have log in the name and are csv format with 3 line header
  if(grepl("SD", file_list[i], fixed=TRUE)) {
    headers <- read_excel(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 2)
  } else {
    headers <- read_csv(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 3)
  }
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  # Input data file
  if(grepl("SD", file_list[i], fixed=TRUE)) {
    df <- read_excel(here(filepath,file_list[i]), col_names = headers, na="..", skip = 2)
  } else {
    df <- read_csv(here(filepath,file_list[i]), col_names = headers, na="..", skip = 3)
  }
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  df_sub <- df[, c(headers[1], headers[which(grepl("System overview PV - DC-coupled", headers, fixed=TRUE) | 
                                           grepl("System overview Battery Power", headers, fixed=TRUE) | 
                                           grepl("Solar Charger PV current", headers, fixed=TRUE) |
                                           grepl("Solar Charger PV voltage", headers, fixed=TRUE) |
                                           grepl("Solar Charger Load current", headers, fixed=TRUE) |
                                           grepl("Battery Monitor State of charge", headers, fixed=TRUE))])] 
  if(grepl("SD", file_list[i], fixed=TRUE)) {
    df_sub <- cbind(df_sub, df[, headers[which(grepl("Solar Charger Battery current", headers, fixed=TRUE) | 
                                        grepl("Solar Charger Battery voltage", headers, fixed=TRUE))]])
    df_sub <- df_sub %>% mutate(`Solar Charger Battery watts`=`Solar Charger Battery voltage (V)`* 
                                  `Solar Charger Battery current (A)`)
    df_sub <- df_sub[,-c(8,9)]
  } else {
    df_sub <- cbind(df_sub, df[, headers[which(grepl("Solar Charger Battery watts", headers, fixed=TRUE))]])
  }
  
  # Adjust timestamp if needed and rename the columns
  if(grepl("GMT", colnames(df_sub)[1], fixed=TRUE)) {
    df_sub[,1] <- df_sub[,1] %m+% hours(5) %m+% minutes(45)
  } else if(grepl("India", colnames(df_sub)[1], fixed=TRUE)) {
    df_sub[,1] <- df_sub[,1] %m+% minutes(15)
  }
  
  if(grepl("SD", file_list[i], fixed=TRUE)) {
    sysOverview <- data.frame(timestamp=df_sub[,1], 
                              System.overview.PV...DC.coupled.W=df_sub[,"System overview PV - DC-coupled (W)"],
                              System.overview.Battery.Power.W=df_sub[,"System overview Battery Power (System) (W)"])
    solarCharger <- data.frame(Solar.Charger.Load.current.A=df_sub[,"Solar Charger Load current (A)"],
                               Solar.Charger.PV.voltage.V=df_sub[,"Solar Charger PV voltage"],
                               Solar.Charger.PV.current.A=df_sub[,"Solar Charger PV current"],
                               Solar.Charger.Battery.watts.W=df_sub[,"Solar Charger Battery watts"])
    batteryMonitor <- data.frame(Battery.Monitor.State.of.charge..=df_sub[,"Battery Monitor State of charge (%)"])
  } else {
    sysOverview <- data.frame(timestamp=df_sub[,1], 
                              System.overview.PV...DC.coupled.W=df_sub[,"System overview PV - DC-coupled W"],
                              System.overview.Battery.Power.W=df_sub[,"System overview Battery Power W"])
    solarCharger <- data.frame(Solar.Charger.Load.current.A=df_sub[,"Solar Charger Load current A"],
                               Solar.Charger.PV.voltage.V=df_sub[,"Solar Charger PV voltage "],
                               Solar.Charger.PV.current.A=df_sub[,"Solar Charger PV current "],
                               Solar.Charger.Battery.watts.W=df_sub[,"df[, headers[which(grepl(\"Solar Charger Battery watts\", headers, "])
    batteryMonitor <- data.frame(Battery.Monitor.State.of.charge..=df_sub[,"Battery Monitor State of charge %"])
  }
  
  systemData <- sysOverview
  systemData <- cbind(systemData, solarCharger, batteryMonitor)
  
  # Calculate PV power, add the streetlight id, remove PV current and PV voltage and re-arrange columns
  systemData <- systemData %>% mutate(Solar.Charger.PV.power.W=Solar.Charger.PV.voltage.V*Solar.Charger.PV.current.A,
                              streetlight=substr(file_list[i],6,8))
  systemData <- systemData[,-c(5,6)]
  systemData <- systemData[,c(1,8,6,5,4,3,2,7)]

  sl_nepal2 <- rbind(sl_nepal2, systemData)
}
write.csv(sl_nepal2, file=here(output_directory,"sl_all_raw_oct_mar.csv"), row.names=FALSE)
#*************************************************************************************************************#

#************* Data read in sl_nepal1 and sl_nepal2 *************#
#SL1 <- 2019-06-18 to 2019-11-14 and 2019-11-01 to 2020-03-15
#SL2 <- 2019-06-18 to 2019-10-16 and 2019-09-30 to 2020-03-05
#SL3 <- 2019-06-28 to 2019-10-16 and 2019-09-30 to 2020-03-05
#SL4 <- 2019-06-18 to 2019-10-16 and 2019-09-08 to 2020-03-05
#SL5 <- 2019-06-18 to 2019-08-27
#SL6 <- 2019-07-01 to 2019-11-14 and 2019-10-05 to 2020-03-05
#SL7 <- 2019-06-18 to 2019-10-16 and 2020-01-28 to 2020-03-05
#** Read in missing data ranges between 1st July 2019 to 19th March 2020 *************#
#SL1 <- 2020-03-15 to 2020-03-19; SL2, SL3, SL4, SL6 <- 2020-03-05 to 2020-03-19
#SL5 <- 2019-08-28 to 2020-03-19 - NA; SL7 <- 2019-10-16 to 2020-01-28 and 2020-03-05 to 2020-03-19

#*************************************************************************************************************#
# Set working directory to read all files from 30th Nov 2019 to 19 March 2020
filepath <- "Data/Oct2019_Mar2020"
file_list <- list.files(here(filepath))
sl_nepal3 <- data.frame()
for(k in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[k]), col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  df <- read_csv(here(filepath,file_list[k]), col_names = headers, na="..", skip = 3)
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  # Select dates for the system_hourly data
  if(k==1) {
    df <- df[date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-15" &
               date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19",]
  } else if(k==2 | k==3 | k==4 | k==5) {
    df <- df[date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-05" &
               date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19",]
  } else if(k==6) {
    df <- df[(date(df$`timestamp Asia/Katmandu (+05:45) `)>="2019-10-16" &
                date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-01-28") | 
               (date(df$`timestamp Asia/Katmandu (+05:45) `)>="2020-03-05" & 
                  date(df$`timestamp Asia/Katmandu (+05:45) `)<="2020-03-19"),]
  }
  
  df <- df[, c(headers[1], headers[which(grepl("System overview PV - DC-coupled", headers, fixed=TRUE) | 
                                               grepl("System overview Battery Power", headers, fixed=TRUE) | 
                                               grepl("Solar Charger PV current", headers, fixed=TRUE) |
                                               grepl("Solar Charger PV voltage", headers, fixed=TRUE) |
                                               grepl("Solar Charger Load current", headers, fixed=TRUE) |
                                                grepl("Solar Charger Battery watts", headers, fixed=TRUE) |
                                               grepl("Battery Monitor State of charge", headers, fixed=TRUE) )])] 
  df <- df %>% mutate("Solar.Charger.PV.power.W"=`Solar Charger PV voltage ` * `Solar Charger PV current `, 
                      streetlight=substr(file_list[k],6,8))
  df <- df[,-c(5,6)]
  
  # Rename and re-arrange the columns
  colnames(df) <- c("timestamp","Battery.Monitor.State.of.charge..","Solar.Charger.Battery.watts.W",
                    "Solar.Charger.Load.current.A", "System.overview.PV...DC.coupled.W", 
                    "System.overview.Battery.Power.W","Solar.Charger.PV.power.W","streetlight")
  df <- df[,c(1,8,2,3,4,6,5,7)]
  
  sl_nepal3 <- rbind(sl_nepal3, df)
}
write.csv(sl_nepal3, file=here(output_directory,"sl_all_raw_rem_mar.csv"), row.names=FALSE)
#*************************************************************************************************************#

#*************************************************************************************************************#
# Set working directory to read all subsequent data from 19 March 2020 onwards
filepath <- "Data/2020"
file_list <- list.files(here(filepath))
sl_nepal4 <- data.frame()
for(k in seq_along(file_list)) {
  headers <- read_csv(here(filepath,file_list[k]), col_names = FALSE, na="..", n_max = 3)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  headers <- sub("\\[.*\\] ", "", headers)
  
  df <- read_csv(here(filepath,file_list[k]), col_names = headers, na="..", skip = 3)
  df <- as.data.frame(df)
  df <- df[,seq_along(df)]
  
  df <- df[, c(headers[1], headers[which(grepl("System overview PV - DC-coupled", headers, fixed=TRUE) | 
                                           grepl("System overview Battery Power", headers, fixed=TRUE) | 
                                           grepl("Solar Charger PV current", headers, fixed=TRUE) |
                                           grepl("Solar Charger PV voltage", headers, fixed=TRUE) |
                                           grepl("Solar Charger Load current", headers, fixed=TRUE) |
                                           grepl("Solar Charger Battery watts", headers, fixed=TRUE) |
                                           grepl("Battery Monitor State of charge", headers, fixed=TRUE) )])] 
  df <- df %>% mutate("Solar.Charger.PV.power.W"=`Solar Charger PV voltage ` * `Solar Charger PV current `, 
                      streetlight=substr(file_list[k],6,8))
  df <- df[,-c(5,6)]
  
  # Rename and re-arrange the columns
  colnames(df) <- c("timestamp","Battery.Monitor.State.of.charge..","Solar.Charger.Battery.watts.W",
                    "Solar.Charger.Load.current.A", "System.overview.PV...DC.coupled.W", 
                    "System.overview.Battery.Power.W","Solar.Charger.PV.power.W","streetlight")
  df <- df[,c(1,8,2,3,4,6,5,7)]
  
  sl_nepal4 <- rbind(sl_nepal4, df)
}
write.csv(sl_nepal4, file=here(output_directory,"sl_all_raw_2020.csv"), row.names=FALSE)
#*************************************************************************************************************#

#*************************************************************************************************************#
# Stitch all data together together
sl_nepal <- read.csv(here(output_directory,"sl_all_raw_jul_oct.csv"), stringsAsFactors =FALSE, header=TRUE)
sl_nepal2 <- read.csv(here(output_directory,"sl_all_raw_oct_mar.csv"), stringsAsFactors =FALSE, header=TRUE)
sl_nepal3 <- read.csv(here(output_directory,"sl_all_raw_rem_mar.csv"), stringsAsFactors =FALSE, header=TRUE)
sl_nepal4 <- read.csv(here(output_directory,"sl_all_raw_2020.csv"), stringsAsFactors =FALSE, header=TRUE)

sl_all <- data.frame()
sl_all <- rbind(sl_all, sl_nepal, sl_nepal2, sl_nepal3, sl_nepal4)
sl_all <- distinct(sl_all)
# Subset data from 1st July 2019 to 31st March 2020
sl_all <- sl_all %>% mutate(timestamp=as.POSIXct(timestamp, origin="1970-01-01",tz="GMT"),date=date(timestamp))
sl_all <- sl_all[sl_all$date>="2019-07-01" & sl_all$date<="2020-03-31",]
write.csv(sl_all, file=here(output_directory,"sl_all_raw.csv"), row.names=FALSE)
#*************************************************************************************************************#

#*************************************************************************************************************#
# Set working directory to read weather data
filepath <- "Data/Weather data"
file_list <- list.files(here(filepath))
weather <- data.frame()
for(i in seq_along(file_list)) {
  headers <- read_excel(here(filepath,file_list[i]), col_names = FALSE, na="..", n_max = 2)
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  
  weather_data <- read_excel(here(filepath,file_list[i]), col_names = headers, na="..", skip = 2,
                             col_types=c("date","date","numeric","numeric"))
  weather_data <- weather_data[,-c(1,3)]
  weather_data$`In-plane PV Incident Solar kW/m2` <- weather_data$`In-plane PV Incident Solar kW/m2`* AREA*EFF*1000.0
  
  s <- unlist(strsplit(gsub("_20.*","",file_list[i]), "_"))
  for(j in 1:(length(s)-1)) {
    weather_data <- cbind(weather_data, weather_data$`In-plane PV Incident Solar kW/m2`)
  }
  colnames(weather_data) <- c("timestamp",s)
  weather_data <- gather(weather_data, "streetlight", "value", 2:length(weather_data))
  
  weather <- rbind(weather, weather_data)
}
weather_data <- spread(weather, streetlight, value)
weather_data <- weather_data[complete.cases(weather_data), ]
# Subset data after 1st July 2019
weather_data <- weather_data[weather_data$timestamp>="2019-07-01 00:00:00 UTC",]
weather_data <- weather_data[-1,]
write.csv(weather_data, file=here(output_directory,"weather_hourly_jul_mar.csv"), row.names=FALSE)
#*************************************************************************************************************#