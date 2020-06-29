#******************************************************************************************#
# This is the script for stitching data from all Nepal SL together from July 2019 onwards  #
# Author: K Bhargava                                                                       #
# Last updated on: 26th June 2020                                                          #
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

#*****************************************************************************************#
filepath <- "Data/Jul19_Jun20"
file_list <- list.files(here(filepath))
sl_all <- data.frame()
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
    df_sub[,1] <- as.POSIXct(as.character(df_sub[,1] %m+% hours(5) %m+% minutes(45)), 
                             tz="Asia/Kathmandu", origin="1970-01-01")
  } else if(grepl("Berlin", colnames(df_sub)[1], fixed=TRUE)) {
    df_sub[,1] <- as.POSIXct(as.character(df_sub[,1] %m+% hours(4) %m+% minutes(45)), 
                             tz="Asia/Kathmandu", origin="1970-01-01")
  } else if(grepl("Mauritius", colnames(df_sub)[1], fixed=TRUE)) {
    df_sub[,1] <- as.POSIXct(as.character(df_sub[,1] %m+% hours(1) %m+% minutes(45)), 
                             tz="Asia/Kathmandu", origin="1970-01-01")
  } else {
    df_sub[,1] <- as.POSIXct(as.character(df_sub[,1]), tz="Asia/Kathmandu", origin="1970-01-01")
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
  
  sl_all <- rbind(sl_all, systemData)
}
sl_all <- distinct(sl_all)
# Subset data between 01 July 2019 to 31 Mar 2020
sl_all <- sl_all %>% mutate(date=date(timestamp))
sl_all <- sl_all[sl_all$date>="2019-07-01" & sl_all$date<="2020-03-31",]
sl_all <- sl_all[,-9] # Remove date
write.csv(sl_all, file=here(output_directory,"sl_all_raw_jul19_mar20.csv"), row.names=FALSE)
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
  weather_data <- weather_data %>% 
    mutate(timestamp=as.POSIXct(as.character(timestamp), tz="Asia/Kathmandu",origin="1970-01-01"))
  weather_data <- gather(weather_data, "streetlight", "value", 2:length(weather_data))
  
  weather <- rbind(weather, weather_data)
}
weather_data <- spread(weather, streetlight, value)
weather_data <- weather_data[complete.cases(weather_data), ]
# Subset data between 01 july 2019 and 31 mar 2020
weather_data <- weather_data %>% mutate(date=date(timestamp))
weather_data <- weather_data[weather_data$date>="2019-07-01" & weather_data$date<="2020-03-31",]
weather_data <- weather_data[,-9] #Remove date
write.csv(weather_data, file=here(output_directory,"weather_hourly_jul19_mar20.csv"), row.names=FALSE)
#*************************************************************************************************************#