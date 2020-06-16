library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)
library(openxlsx)

### Read in data for all SL - read in stitched data till Oct and then onwards ##
#Extract values System overview Battery Power, System overview PV-DC-coupled,
#Solar Charger PV Power, Battery monitor state of charge 
#Battery Monitor Discharged energy and Battery Monitor Charged Energy 
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Full/")
file_list <- list.files()
sl <- data.frame()
for(k in 1:length(file_list)) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  
  headers = colnames(df)
  #Extract values for System overview Battery Power and System overview PV-DC-coupled
  #Find all column names with System overview
  columns <- headers[which(grepl("System.overview", headers, fixed=TRUE))]
  #Find all column names with PV - DC-coupled W
  colNames <- columns[which(grepl("PV...DC.coupled", columns, fixed=TRUE))]
  #Find all column names with Battery Power W
  colNames <- c(colNames, columns[which(grepl("Battery.Power", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  sysOverview <- df[,colNames]
  
  #Extract values for Solar Charger PV power
  #Find all column names with Solar Charger
  columns <- headers[which(grepl("Solar.Charger", headers, fixed=TRUE))]
  #Find all column names with PV power
  colNames <- columns[which(grepl("PV.power", columns, fixed=TRUE))]
  #Find all column names with PV.current
  colNames <- c(colNames, columns[which(grepl("PV.current", columns, fixed=TRUE))])
  #Find all column names with PV.voltage
  colNames <- c(colNames, columns[which(grepl("PV.voltage", columns, fixed=TRUE))])
  #Find all column names with Battery.current
  colNames <- c(colNames, columns[which(grepl("Battery.current", columns, fixed=TRUE))])
  #Find all column names with Battery.voltage
  colNames <- c(colNames, columns[which(grepl("Battery.voltage", columns, fixed=TRUE))])
  #Find all column names with Battery.watts
  colNames <- c(colNames, columns[which(grepl("Battery.watts", columns, fixed=TRUE))])
  #Find all column names with Charger.Voltage
  colNames <- c(colNames, columns[which(grepl("Charger.Voltage", columns, fixed=TRUE))])
  #Find all column names with Load.current
  colNames <- c(colNames, columns[which(grepl("Load.current", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  solarCharger$Solar.Charger.PV.power <- 
    solarCharger$Solar.Charger.PV.current * solarCharger$Solar.Charger.PV.voltage
  solarCharger <- solarCharger[,-c(3,4)] #Remove PV current and voltage
  if(length(solarCharger)==7) {
    solarCharger$Solar.Charger.Battery.watts.W <- 
      ifelse(is.na(solarCharger$Solar.Charger.Battery.watts.W),
             solarCharger$Solar.Charger.Battery.current.A * 
               solarCharger$Solar.Charger.Battery.voltage.V,
             solarCharger$Solar.Charger.Battery.watts.W)
    solarCharger <- solarCharger[,-c(3,4)] #Remove Battery current and voltage
  }

  #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
  #Find all column names with Battery Monitor
  columns <- headers[which(grepl("Battery.Monitor", headers, fixed=TRUE))]
  #Find all column names with State of charge
  colNames <- columns[which(grepl("State.of.charge", columns, fixed=TRUE))]
  #Find all column names with Discharged Energy
  colNames <- c(colNames, columns[which(grepl("Discharged.Energy", columns, fixed=TRUE))])
  #Find all column names with Charged Energy
  colNames <- c(colNames, columns[which(grepl("Charged.Energy", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  batteryMonitor <- df[,colNames]
  
  systemData <- data.frame()
  systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  #Time is in Nepal (GMT+5:45) - just parsing time to time in GMT
  if(k!=6) {
    systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01")
  } else {
    systemData$timestamp <- as.POSIXct(systemData$timestamp,format="%d/%m/%Y %H:%M", tz="GMT")
  }
  
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W", "Battery_watts_W", "State_of_charge",
                            "Discharged_energy_kWh","Charged_energy_kWh")
  
  ######Analyse data to get hourly values #######################################
  system_gather <- gather(systemData, "id", "value", 2:8)
  system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
  system_gather$date <- date(system_gather$timestamp)
  
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | 
                                   system_gather$id=="PV_power_W" |
                                   system_gather$id=="Battery_power_W" |
                                   system_gather$id=="Battery_watts_W", ] %>%
    group_by(date,timeUse,id) %>%
    summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
  
  #Calculate the last value in an hour for state of charge
  system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
    group_by(date,timeUse,id) %>%
    summarise(value=mean(value, na.rm=TRUE))
  system_soc <- as.data.frame(system_soc)
  system_hourly <- rbind(system_hourly, system_soc)
  
  #Calculate hourly values for discharged and charged energy by taking hourly differences
  battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
  battery_charge <- battery_charge[complete.cases(battery_charge), ]
  #Extract hourly values by taking the last value for each hour 
  battery_charge_hours <- battery_charge %>%
    group_by(date, timeUse, id) %>%
    summarise(value = value[length(na.omit(value))])
  battery_charge_hours <- as.data.frame(battery_charge_hours)
  a <- diff(battery_charge_hours$value)
  battery_charge_hours <- battery_charge_hours[-1,]
  battery_charge_hours$value <- a
  battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_charge_hours)
  
  battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
  battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
  #Extract hourly values by taking the max value each hour 
  battery_discharge_hours <- battery_discharge %>%
    group_by(date, timeUse, id) %>%
    summarise(value = value[length(na.omit(value))])
  battery_discharge_hours <- as.data.frame(battery_discharge_hours)
  a <- diff(battery_discharge_hours$value)
  battery_discharge_hours <- battery_discharge_hours[-1,]
  battery_discharge_hours$value <- a
  battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_discharge_hours)
  
  #Add the Streetlight ID to the data
  system_hourly$streetlight <- rep(gsub("_.*","",file_list[k]), length(system_hourly$date))
  
  #Bind to sl data frame
  sl <- rbind(sl, system_hourly)
}

############################ Read in data after the one stitched #############################
#For SL1 - Nov, Dec, Jan and Feb
#For SL2, SL4, SL7 - Feb
#For SL3 and SL6 - Dec and Feb
#For SL5 - no files atm
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl2 <- data.frame()
for(k in 1:6) {
  #Read files for each SL
  setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
  if(k==1) {
    setwd("./SL1/")
  } else if(k==2) {
    setwd("./SL2/")
  } else if(k==3) {
    setwd("./SL3/")
  } else if(k==4) {
    setwd("./SL4/")
  } else if(k==5) {
    setwd("./SL6/")
  } else if(k==6) {
    setwd("./SL7/")
  }

  #Input the file for each SL
  file_list <- list.files()
  #SD card files have SD in it and are xlsx - 2 line header
  #Victron files have log in it and are csv - 3 line header
  for(i in 1:length(file_list)) {
    name <- file_list[i]
    
    #Check for the phrase SD in the name 
    if(grepl("SD", name, fixed=TRUE)) {
      #Concatenate the headers spread across first 2 rows
      headers <- read_excel(file_list[i], col_names = FALSE, na="..", n_max = 2)
    } else {
      #Concatenate the headers spread across first 3 rows
      headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
    }
    #Replace NA in header with "" for missing row 3 values
    headers[is.na(headers)] <- ""
    column_labels <- headers %>% summarize_all(str_c, collapse = " ")
    headers = unname(unlist(column_labels[1,]))
    
    #Check for the phrase SD in the name 
    if(grepl("SD", name, fixed=TRUE)) {
      #Read data without the first two rows
      df <- read_excel(file_list[i], col_names = headers, na="..", skip = 2)
      #if(class(df$`Timestamp GMT Standard Time`)=="numeric") {
      #  df$`Timestamp GMT Standard Time` <- convertToDateTime(df$`Timestamp GMT Standard Time`,
      #                                                        tz="GMT")
      #}
    } else {
      #Read data without the first three rows
      df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
    }
    #Replace NA in data frame with "" for missing values as in raw file
    df[is.na(df)] <- ""
    
    #Extract values for System overview Battery Power and System overview PV-DC-coupled
    #Find all column names with System overview
    columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
    #Find all column names with PV - DC-coupled W
    colNames <- columns[which(grepl("PV - DC-coupled", columns, fixed=TRUE))]
    #Find all column names with Battery Power W
    colNames <- c(colNames, columns[which(grepl("Battery Power", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    sysOverview <- df[,colNames]
    sysOverview <- as.data.frame(sysOverview)
    sysOverview <- sysOverview[,c(1:length(sysOverview))]
    
    #Extract values for Solar Charger PV power
    #Find all column names with Solar Charger
    columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
    #Find all column names with PV current
    colNames <- columns[which(grepl("PV current", columns, fixed=TRUE))]
    #Find all column names with PV voltage
    colNames <- c(colNames, columns[which(grepl("PV voltage", columns, fixed=TRUE))])
    #Read the load current
    colNames <- c(colNames, columns[which(grepl("Load current", columns, fixed=TRUE))])
    if(grepl("SD", name, fixed=TRUE)) {
      #Find all columns with Battery current
      colNames <- c(colNames, columns[which(grepl("Battery current", columns, fixed=TRUE))])
      #Find all column names with Battery voltage
      colNames <- c(colNames, columns[which(grepl("Battery voltage", columns, fixed=TRUE))])
    } else {
      #Check for the phrase SD in the name - if not read Battery watts 
      colNames <- c(colNames, columns[which(grepl("Battery watts", columns, fixed=TRUE))])
      #Read Voltage V
      colNames <- c(colNames, columns[which(grepl("Voltage V", columns, fixed=TRUE))])
    }
    colNames <- c(headers[1], colNames) 
    solarCharger <- df[,colNames]
    solarCharger <- as.data.frame(solarCharger)
    solarCharger <- solarCharger[,c(1:length(solarCharger))]
    if(grepl("SD", name, fixed=TRUE)) {
      colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                                  "Battery_current_A","Battery_voltage_V")
      solarCharger$Battery_watts_W <- 
        solarCharger$Battery_current_A * solarCharger$Battery_voltage_V
      solarCharger$Light_load_W <- 
        solarCharger$Load_current_A * solarCharger$Battery_voltage_V
      solarCharger <- solarCharger[,-c(4,5,6)] #Remove Load current, Battery current and battery voltage
    } else {
      colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                                  "Battery_watts_W", "Charger_voltage_V")
      solarCharger$Light_load_W <- 
        solarCharger$Load_current_A * solarCharger$Charger_voltage_V
      solarCharger <- solarCharger[,-c(4,6)] #Remove Load current and charger voltage
    }
    solarCharger$PV_power_W <- 
      solarCharger$PV_current_A * solarCharger$PV_voltage_V
    solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
    solarCharger <- solarCharger[,c(1,4,2:3)] #Rearrange columns
    
    #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
    #Find all column names with Battery Monitor
    columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
    #Find all column names with State of charge
    colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
    #Find all column names with Discharged Energy
    colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
    #Find all column names with Charged Energy
    colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    batteryMonitor <- df[,colNames]
    batteryMonitor <- as.data.frame(batteryMonitor)
    batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
    
    systemData <- data.frame()
    systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
    #Check for the timezone of the timestamp - convert to Kathmandu = GMT+5:45
    if(grepl("GMT", colnames(systemData)[1], fixed=TRUE)) {
      systemData[,1] <- systemData[,1] %m+% hours(5) %m+% minutes(45)
    } else if(grepl("India", colnames(systemData)[1], fixed=TRUE)) {
      systemData[,1] <- systemData[,1] %m+% hours(1)
    }
    colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                              "PV_power_W", "Battery_watts_W", "Light_load_W",
                              "State_of_charge","Discharged_energy_kWh",
                              "Charged_energy_kWh")
    systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
    systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
    systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
    systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
    
    systemData$month <- as.character(month(systemData$timestamp, label=TRUE, abbr=TRUE))
    
    #Remove rows where the light load is 0 between 18:00 and 6:00
    systemData$time <- as.numeric(format(systemData$timestamp, format='%H'))
    systemData <- systemData[systemData$Light_load_W!=0 | (systemData$Light_load_W==0 
                                                        & (systemData$time>6 & systemData$time<18)), ]
    systemData <- systemData[,-11] #Remove time
    
    ######Analyse data to get hourly values #######################################
    system_gather <- gather(systemData, "id", "value", c(2:9))
    system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
    system_gather$date <- date(system_gather$timestamp)
    
    #Hourly means can be calculated for AC consumption and PV power
    system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | 
                                     system_gather$id=="PV_power_W" |
                                     system_gather$id=="Battery_power_W" |
                                     system_gather$id=="Battery_watts_W" |
                                     system_gather$id=="Light_load_W", ] %>%
      group_by(date,timeUse,id) %>%
      summarise(value=mean(value,na.rm = TRUE))
    system_hourly <- as.data.frame(system_hourly)
    
    #Calculate the last value in an hour for state of charge
    system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
      group_by(date,timeUse,id) %>%
      summarise(value=mean(value, na.rm=TRUE))
    system_soc <- as.data.frame(system_soc)
    system_hourly <- rbind(system_hourly, system_soc)
    
    #Calculate hourly values for discharged and charged energy by taking hourly differences
    battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
    battery_charge <- battery_charge[complete.cases(battery_charge), ]
    #Extract hourly values by taking the last value for each hour 
    battery_charge_hours <- battery_charge %>%
      group_by(date, timeUse, id) %>%
      summarise(value = value[length(na.omit(value))])
    battery_charge_hours <- as.data.frame(battery_charge_hours)
    a <- diff(battery_charge_hours$value)
    battery_charge_hours <- battery_charge_hours[-1,]
    battery_charge_hours$value <- a
    battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
    system_hourly <- rbind(system_hourly, battery_charge_hours)
    
    battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
    battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
    #Extract hourly values by taking the max value each hour 
    battery_discharge_hours <- battery_discharge %>%
      group_by(date, timeUse, id) %>%
      summarise(value = value[length(na.omit(value))])
    battery_discharge_hours <- as.data.frame(battery_discharge_hours)
    a <- diff(battery_discharge_hours$value)
    battery_discharge_hours <- battery_discharge_hours[-1,]
    battery_discharge_hours$value <- a
    battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
    system_hourly <- rbind(system_hourly, battery_discharge_hours)
    
    #Add the Streetlight ID to the data
    system_hourly$streetlight <- rep(substr(name,6,8), length(system_hourly$date))
    
    #Bind to sl data frame
    sl2 <- rbind(sl2, system_hourly)
  }
}

##################################################################################
#Checking dates for sl and sl2 for each streetlight to identify gaps
sl_spread <- spread(sl, id, value)
sl2_spread <- sl2
sl2_spread$date_time_id_sl <- paste(sl2_spread$date, sl2_spread$timeUse,
                                    sl2_spread$id, sl2_spread$streetlight)
sl2_spread <- sl2_spread[-which(duplicated(sl2_spread$date_time_id_sl)==TRUE),]
sl2_spread <- sl2_spread[,-6]
sl2_spread <- spread(sl2_spread, id, value)

sl1_1 <- range(sl_spread$date[sl_spread$streetlight=="SL1"])
sl1_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL1"])
sl2_1 <- range(sl_spread$date[sl_spread$streetlight=="SL2"])
sl2_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL2"])
sl3_1 <- range(sl_spread$date[sl_spread$streetlight=="SL3"])
sl3_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL3"])
sl4_1 <- range(sl_spread$date[sl_spread$streetlight=="SL4"])
sl4_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL4"])
sl5_1 <- range(sl_spread$date[sl_spread$streetlight=="SL5"])
sl5_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL5"])
sl6_1 <- range(sl_spread$date[sl_spread$streetlight=="SL6"])
sl6_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL6"])
sl7_1 <- range(sl_spread$date[sl_spread$streetlight=="SL7"])
sl7_2 <- range(sl2_spread$date[sl2_spread$streetlight=="SL7"])

################################################################################
############ Included data ranges in sl and sl2 ##############################
#SL1 <- 2019-06-18 to 2019-11-14 and 2019-11-01 to 2020-03-15
#SL2 <- 2019-06-18 to 2019-10-16 and 2019-09-30 to 2020-03-05
#SL3 <- 2019-06-28 to 2019-10-16 and 2019-09-30 to 2020-03-05
#SL4 <- 2019-06-18 to 2019-10-16 and 2019-09-08 to 2020-03-05
#SL5 <- 2019-06-18 to 2019-08-27
#SL6 <- 2019-07-01 to 2019-11-14 and 2019-10-05 to 2020-03-05
#SL7 <- 2019-06-18 to 2019-10-16 and 2020-01-28 to 2020-03-05

### Read in missing data ranges between 1st July 2019 to 19th March 2020 ########
#SL1 <- 2020-03-15 to 2020-03-19
#SL2 <- 2020-03-05 to 2020-03-19
#SL3 <- 2020-03-05 to 2020-03-19
#SL4 <- 2020-03-05 to 2020-03-19
#SL5 <- 2019-08-28 to 2020-03-19 ---- nothing is available
#SL6 <- 2020-03-05 to 2020-03-19
#SL7 <- 2019-10-16 to 2020-01-28 and 2020-03-05 to 2020-03-19

setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Oct2019_Mar2020/")
file_list <- list.files()
sl3 <- data.frame()
for(k in 1:length(file_list)) {
    headers <- read_csv(file_list[k], col_names = FALSE, na="..", n_max = 3)
    #Replace NA in header with "" for missing row 3 values
    headers[is.na(headers)] <- ""
    column_labels <- headers %>% summarize_all(str_c, collapse = " ")
    headers = unname(unlist(column_labels[1,]))
    
    df <- read_csv(file_list[k], col_names = headers, na="..", skip = 3)
    df[is.na(df)] <- ""
    
    #Select dates for the system_hourly data
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
    
    columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV - DC-coupled", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Battery Power", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    sysOverview <- df[,colNames]
    sysOverview <- as.data.frame(sysOverview)
    sysOverview <- sysOverview[,c(1:length(sysOverview))]
    
    columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV current", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("PV voltage", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Load current", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Battery watts", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Voltage V", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    solarCharger <- df[,colNames]
    solarCharger <- as.data.frame(solarCharger)
    solarCharger <- solarCharger[,c(1:length(solarCharger))]
    colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                                "Battery_watts_W", "Charger_voltage_V")
    solarCharger$Light_load_W <- 
      solarCharger$Load_current_A * solarCharger$Charger_voltage_V
    solarCharger <- solarCharger[,-c(4,6)] #Remove Load current and charger voltage
    solarCharger$PV_power_W <- 
      solarCharger$PV_current_A * solarCharger$PV_voltage_V
    solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
    solarCharger <- solarCharger[,c(1,4,2:3)] #Rearrange columns
    
    columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    batteryMonitor <- df[,colNames]
    batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
    
    systemData <- data.frame()
    systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
    colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                              "PV_power_W", "Battery_watts_W", "Light_load_W",
                              "State_of_charge","Discharged_energy_kWh",
                              "Charged_energy_kWh")
    
    systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
    systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
    systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
    systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
    systemData$State_of_charge <- as.numeric(systemData$State_of_charge)
    
    systemData$month <- as.character(month(systemData$timestamp, label=TRUE, abbr=TRUE))
    ######Analyse data to get hourly values #######################################
    system_gather <- gather(systemData, "id", "value", c(2:9))
    system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
    system_gather$date <- date(system_gather$timestamp)
    
    #Hourly means can be calculated for AC consumption and PV power
    system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | 
                                     system_gather$id=="PV_power_W" |
                                     system_gather$id=="Battery_power_W" |
                                     system_gather$id=="Battery_watts_W" |
                                     system_gather$id=="Light_load_W", ] %>%
      group_by(date,timeUse,id) %>%
      summarise(value=mean(value,na.rm = TRUE))
    system_hourly <- as.data.frame(system_hourly)
    
    #Calculate the last value in an hour for state of charge
    system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
      group_by(date,timeUse,id) %>%
      summarise(value=mean(value, na.rm=TRUE))
    system_soc <- as.data.frame(system_soc)
    system_hourly <- rbind(system_hourly, system_soc)
    
    #Calculate hourly values for discharged and charged energy by taking hourly differences
    battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
    battery_charge <- battery_charge[complete.cases(battery_charge), ]
    #Extract hourly values by taking the last value for each hour 
    battery_charge_hours <- battery_charge %>%
      group_by(date, timeUse, id) %>%
      summarise(value = value[length(na.omit(value))])
    battery_charge_hours <- as.data.frame(battery_charge_hours)
    a <- diff(battery_charge_hours$value)
    battery_charge_hours <- battery_charge_hours[-1,]
    battery_charge_hours$value <- a
    battery_charge_hours$value <- battery_charge_hours$value * 1000.0 #W
    system_hourly <- rbind(system_hourly, battery_charge_hours)
    
    battery_discharge <- system_gather[system_gather$id=="Discharged_energy_kWh",]
    battery_discharge <- battery_discharge[complete.cases(battery_discharge), ]
    #Extract hourly values by taking the max value each hour 
    battery_discharge_hours <- battery_discharge %>%
      group_by(date, timeUse, id) %>%
      summarise(value = value[length(na.omit(value))])
    battery_discharge_hours <- as.data.frame(battery_discharge_hours)
    a <- diff(battery_discharge_hours$value)
    battery_discharge_hours <- battery_discharge_hours[-1,]
    battery_discharge_hours$value <- a
    battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
    system_hourly <- rbind(system_hourly, battery_discharge_hours)
    
    #Add the Streetlight ID to the data
    system_hourly$streetlight <- rep(substr(file_list[k],6,8), length(system_hourly$date))
    
    #Bind to sl data frame
    sl3 <- rbind(sl3, system_hourly)
}

###############################################################################
####Combine sl, sl2 and sl3
sl_all <- data.frame()
sl_all <- rbind(sl_all, sl, sl2, sl3)
sl_all <- sl_all[order(sl_all$date),]
sl_all$date_time_id_sl <- paste(sl_all$date, sl_all$timeUse, sl_all$id, 
                                sl_all$streetlight, sep="")
sl_all <- sl_all[-which(duplicated(sl_all$date_time_id_sl)==TRUE),] #Remove duplicated rows
sl_all <- sl_all[,-6] #Remove date_time_id_sl
sl_write <- spread(sl_all, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:01",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
#Remove data before July 2019
sl_write <- sl_write[sl_write$date>=as.Date("2019-07-01"),]

#Replace NA values for charged and discharged energy with 0
sl_write$Charged_energy_kWh[is.na(sl_write$Charged_energy_kWh)] <- 0
sl_write$Discharged_energy_kWh[is.na(sl_write$Discharged_energy_kWh)] <- 0

sl_all <- sl_write
sl_write <- sl_write[,-c(1,2)] #Remove date and timeUse
sl_write <- sl_write[, c(9,1:8)]
colnames(sl_write) <- c("timestamp","streetlight","Battery_power_W",
                        "Battery_watts_W", "Charged_energy_W",
                        "Discharged_energy_W", "PV_DC_coupled_W",
                        "PV_power_W", "State_of_charge_%")
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(sl_write, file="SL_all_jul_mar.csv", row.names=FALSE)

###############################################################################
###Read in the system file
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl_all <- read.csv("SL_all_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
sl_all$timestamp <- as.POSIXct(sl_all$timestamp, tz="GMT", origin="1970-01-01")
sl_all$date <- date(sl_all$timestamp)
sl_all$timeUse <- format(sl_all$timestamp, format='%H')

######### For each streetlight see what check for missing data ####################
#Once the values are all read - analyse the data to get missing days and hours
#Data from 1 July 2019 to 19 March 2020
july_2019 <- seq(as.Date("2019-07-01"), as.Date("2019-07-31"), by="days")
aug_2019 <- seq(as.Date("2019-08-01"), as.Date("2019-08-31"), by="days")
sep_2019 <- seq(as.Date("2019-09-01"), as.Date("2019-09-30"), by="days")
oct_2019 <- seq(as.Date("2019-10-01"), as.Date("2019-10-31"), by="days")
nov_2019 <- seq(as.Date("2019-11-01"), as.Date("2019-11-30"), by="days")
dec_2019 <- seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by="days")
jan_2020 <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by="days")
feb_2020 <- seq(as.Date("2020-02-01"), as.Date("2020-02-29"), by="days")
mar_2020 <- seq(as.Date("2020-03-01"), as.Date("2020-03-19"), by="days")
all_days <- c(july_2019, aug_2019, sep_2019, oct_2019, nov_2019, dec_2019,
              jan_2020, feb_2020, mar_2020)

#Create a vector for all 24 hours in a day
all_hours <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                by = "1 hour"), "%H", tz="GMT")
all_hours <- all_hours[-25]

#Number of on hours per day for each streetlight id and each variable
sl_gather <- gather(sl_all, "variable", "value", 3:9)
sl_on_hours <- sl_gather %>%
  group_by(streetlight, date, variable) %>%
  summarise(onHours = length(na.omit(value)))
sl_on_hours <- as.data.frame(sl_on_hours)
#Once we have hours on for each available day, look for missing dates
sl_on_hours2 <- data.frame()
for(i in 1:length(unique(sl_on_hours$streetlight))) {
  df <- sl_on_hours[sl_on_hours$streetlight == unique(sl_on_hours$streetlight)[i], ]
  for(j in 1:length(unique(df$variable))) {
    df_sub <- df[df$variable == unique(df$variable)[j], ]
    for(k in 1:length(all_days)) {
      if(!(all_days[k] %in% df_sub$date)) {
        df_sub <- rbind(df_sub, data.frame(streetlight=df_sub$streetlight[1], date=all_days[k],
                                           variable=df_sub$variable[1], onHours=0))
      }
    }
    sl_on_hours2 <- rbind(sl_on_hours2, df_sub)
  }
}
sl_on_hours2 <- sl_on_hours2[order(sl_on_hours2$date),]
sl_on_hours2$month <- as.character(month(sl_on_hours2$date,label=TRUE, abbr=TRUE))

sl_on_hours2$date <- as.Date(sl_on_hours2$date)
sl_on_hours2$streetlight <- as.factor(sl_on_hours2$streetlight)
sl_on_hours2$variable <- as.factor(sl_on_hours2$variable)
sl_on_hours2 %>%
  ggplot(aes(date, onHours, color=streetlight, shape=variable)) + 
  geom_point() +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "deepskyblue",
                                "darkorange", "dodgerblue4", "firebrick1", 
                                "darkorchid")) +
  labs(title="Hours of data collection per day for each SL at Hall since Jul 2019" ,
       y="Hours of data collection in a day",
       x = "Day of study" ,
       color="CPE", 
       shape="LED") +
  scale_shape_manual(values=c(1,4,1,4,1,4,1)) +
  theme(axis.text.x = element_text(size=8), plot.title = element_text(size=10)) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24), 
                     labels=c("0","2","4","6","8","10","12","14","16","18","20",
                              "22","24"),
                     limits=c(0,24)) +
  scale_x_date(date_minor_breaks = "1 day") +
  theme(legend.box = "vertical", legend.position = "bottom",
        legend.key.size = unit(0.1, "cm"), 
        legend.margin = margin(t=0,r=0,b=0,l=0))

#Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
sl_on_hours2$yield <- sl_on_hours2$onHours * 100.0 / 24.0
sl_on_hours2$id2 <- paste(as.character(sl_on_hours2$streetlight), 
                           as.character(sl_on_hours2$variable))
library(wesanderson)
#Plotting a heat map for CPE
pal <- wes_palette("Zissou1", 100, type = "continuous")
sl_on_hours2[sl_on_hours2$streetlight=="SL1" | sl_on_hours2$streetlight=="SL2",] %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for SL1 and SL2 data in Nepal: 01 Jul'19 - 19 Mar'20" , 
       y="Parameter",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))

sl_on_hours2[sl_on_hours2$streetlight=="SL3" | sl_on_hours2$streetlight=="SL4",] %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for SL3 and SL4 data in Nepal: 01 Jul'19 - 19 Mar'20" , 
       y="Parameter",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))

sl_on_hours2[sl_on_hours2$streetlight=="SL5" | sl_on_hours2$streetlight=="SL6"
             | sl_on_hours2$streetlight=="SL7",] %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for SL5, SL6 and SL7 data in Nepal: 01 Jul'19 - 19 Mar'20" , 
       y="Parameter",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))

###################################################################################################
#Hourly socket and system consumption for streetlight (1-7) in (month). 
#system and socket loads = Battery power - pv dc-coupled
sl_all$day <- weekdays(sl_all$date, abbr=TRUE)
sl_all$month <- as.character(month(sl_all$date, label=TRUE, abbr=TRUE))
sl_all$sysSocketLoad <- -1*(sl_all$Battery_power_W - sl_all$Battery_watts_W)

sl_all_weday <- sl_all[sl_all$day=="Sat" | sl_all$day=="Sun", c(2,10,11,12,13,14)]
sl_all_wday <- sl_all[sl_all$day!="Sat" & sl_all$day!="Sun", c(2,10,11,12,13,14)]

plotMeans <- function(df) {
  df %>%
    ggplot(aes(x=timeUse, y=sysSocketLoad)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)",
         x = "Hour of day") + 
    theme(plot.title = element_text(size=10), legend.position = "none") 
}

####Plots for SL1
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jul'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jan'20")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Mar'20") 

####Plots for SL2
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Jul'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Jul'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(0,25)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Sep'19")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Sep'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Oct'19")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Oct'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Nov'19")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Nov'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Dec'19")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Dec'19") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Jan'20")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Jan'20") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Feb'20")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Feb'20") +
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Mar'20")+
  scale_y_continuous(limits=c(0,25))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Mar'20") +
  scale_y_continuous(limits=c(0,25))

####Plots for SL3
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Jul'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Jul'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(0,20)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Sep'19")+
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Sep'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Oct'19")+
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Oct'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Nov'19")+
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Nov'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Dec'19") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Dec'19") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Jan'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Jan'20") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Feb'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Feb'20") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Mar'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Mar'20") +
  scale_y_continuous(limits=c(0,50))

###Plots for SL4
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Jul'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Jul'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(0,10)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Sep'19")+
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Sep'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Oct'19")+
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Oct'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Nov'19")+
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Nov'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Dec'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Dec'19") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Jan'20")+
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Jan'20") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Feb'20")+
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Feb'20") +
  scale_y_continuous(limits=c(0,10))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Mar'20")+
  scale_y_continuous(limits=c(0,70))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Mar'20") +
  scale_y_continuous(limits=c(0,70))

###Plots for SL5
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL5" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL5 on a weekday in Jul'19") +
  scale_y_continuous(limits=c(0,15))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL5" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL5 on a weekend day in Jul'19") +
  scale_y_continuous(limits=c(0,15))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL5" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL5 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(0,15)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL5" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL5 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(0,15))

####Plots for SL6
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Jul'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Jul'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(0,20)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Sep'19")+
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Sep'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Oct'19")+
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Oct'19") +
  scale_y_continuous(limits=c(0,20))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Nov'19")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Nov'19") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Dec'19") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Dec'19") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Jan'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Jan'20") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Feb'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Feb'20") +
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Mar'20")+
  scale_y_continuous(limits=c(0,50))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Mar'20") +
  scale_y_continuous(limits=c(0,50))

####Plots for SL7
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Jul'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Jul",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Jul'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Aug'19")+
  scale_y_continuous(limits=c(-100,150)) 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Aug",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Aug'19") +
  scale_y_continuous(limits=c(-100,150))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Sep'19")+
  scale_y_continuous(limits=c(-50,75))

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Sep",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Sep'19") +
  scale_y_continuous(limits=c(-50,75))

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Oct",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Nov",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Dec",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Jan",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Jan'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Feb",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Mar",])
p + labs(title="Mean hourly system and socket load at SL on a weekend day in Mar'20")

#############################################################################
###Plotting daytime socket and system usage since commissioning
#Subset sl_all to only have daytime hours - 7am to 4:59pm i.e. >=7 <=16
sl_all$timeUse <- as.numeric(sl_all$timeUse)
sl_all_daytime <- sl_all[sl_all$timeUse>=7 & sl_all$timeUse<=16,]
#Get daily consumption
startDate <- as.Date("2019-06-30")
sl_all_daily <- sl_all_daytime %>%
  group_by(month, streetlight, date) %>%
  summarise(load = sum(sysSocketLoad))
sl_all_daily <- as.data.frame(sl_all_daily)
sl_all_daily$days <- as.numeric(sl_all_daily$date - startDate)
sl_all_daily <- sl_all_daily[order(sl_all_daily$days),]

#From 1st of July to 19th March = 263 days
plotDaily <- function(df) {
  df %>%
    ggplot(aes(x=days, y=load, color="Actual")) +
    geom_point(shape=8) + 
    labs(y="Total energy consumption (Wh)",
         x = "Days since commissioning",
         colour="") + 
    theme(plot.title = element_text(size=8),legend.position = "none") +
    scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260),
                       labels=c("0","20","40","60","80","100","120","140","160","180",
                                "200","220","240","260"))
}

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL1",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL1 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL2",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL2 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL3",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL3 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL4",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL4 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL5",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL5 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL6",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL6 since commissioning: 1st Jul'19 to 19th Mar'20")

p <- plotDaily(sl_all_daily[sl_all_daily$streetlight=="SL7",])
p + labs(title="Daily daytime (7am to 4:59pm) system and socket load at SL7 since commissioning: 1st Jul'19 to 19th Mar'20")

#########################################################################
#Including capture losses in the plot
#Reading in weather data
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
weather_data <- read.csv("weather_hourly_jul_dec.csv", header=TRUE,
                         stringsAsFactors = FALSE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, tz="GMT",
                                     origin="1970-01-01")
weather_gather <- gather(weather_data, "streetlight","PV_incident_W", 2:8)
weather_gather$Pot_PV <- rep("Pot_PV",length(weather_gather$timestamp))
colnames(weather_gather) <- c("timestamp","streetlight","value","variable")

#Subset the sl data from july to dec
sl_jul_dec <- sl_sub[sl_sub$date<"2020-01-01", c(1,2,5,6)]
sl_jul_dec$timeUse <- paste(ifelse(sl_jul_dec$timeUse<10,"0",""), 
                            sl_jul_dec$timeUse,sep="")
sl_jul_dec$timestamp <- as.POSIXct(paste(paste(sl_jul_dec$date, sl_jul_dec$timeUse), ":00:00",sep=""),
           format="%Y-%m-%d %H:%M:%S", tz="GMT")
sl_jul_dec <- sl_jul_dec[,-c(3,4)]
sl_jul_dec <- sl_jul_dec[,c(3,1,2)]
sl_jul_dec$PV_power <- rep("PV_power",length(sl_jul_dec$timestamp))
colnames(sl_jul_dec) <- c("timestamp","streetlight", "value","variable")

##Combining potential PV and actual PV data
captureLosses <- data.frame()
captureLosses <- rbind(captureLosses, sl_jul_dec, weather_gather)
captureLosses <- spread(captureLosses, variable, value)
captureLosses$losses <- captureLosses$Pot_PV - captureLosses$PV_power
captureLosses$month <- as.character(month(captureLosses$timestamp, label=TRUE,
                                          abbr=TRUE))
captureLosses$timeUse <- format(captureLosses$timestamp, format='%H')  
captureLosses$date <- date(captureLosses$timestamp)
#Getting daily capture losses between 7 and 16
captureLosses$timeUse <- as.numeric(captureLosses$timeUse)
captureLosses <- captureLosses[captureLosses$timeUse>=7 & 
                                 captureLosses$timeUse<=16,]
#Get daily consumption
startDate <- as.Date("2019-06-30")
losses_daily <- captureLosses %>%
  group_by(month, streetlight, date) %>%
  summarise(losses = sum(losses, na.rm = TRUE))
losses_daily <- as.data.frame(losses_daily)
losses_daily$days <- as.numeric(losses_daily$date - startDate)
losses_daily <- losses_daily[order(losses_daily$days),]

sl_all_daily_sub <- sl_all_daily[sl_all_daily$date<="2020-01-01",]
#########################################################################
#Reading in weather data
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
weather_data <- read.csv("weather_hourly_jul_dec.csv", header=TRUE,
                         stringsAsFactors = FALSE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, tz="GMT",
                                     origin="1970-01-01")
weather_gather <- gather(weather_data, "streetlight","PV_incident_W", 2:8)
#Convert PV incident from W back to kW/m2 
weather_gather$PV_incident_W <- weather_gather$PV_incident_W / (1.968 * 0.152 * 1000.0)
colnames(weather_gather) <- c("timestamp", "streetlight","PV_incident_kW/m2")
weather_gather$month <- as.character(month(weather_gather$timestamp, abbr=TRUE,
                                           label=TRUE))
##Calculating monthly totals
weather_monthly <- weather_gather %>%
  group_by(month, streetlight) %>%
  summarise(`PV_incident_kW/m2` = sum(`PV_incident_kW/m2`, na.rm=TRUE))

##calculating monthly totals for PV power and utilised yield
sl_sub <- sl_all[, c(2,3,4,7,10,11,13,14)]
sl_sub$yield <- ifelse(sl_sub$Battery_power_W<0, 
                       sl_sub$Battery_power_W,
                       sl_sub$Battery_power_W - sl_sub$PV_DC_coupled_W)
sl_gather <- gather(sl_sub, "variable", "value", c(2:4, 8:9))
test <- sl_gather[sl_gather$streetlight=="SL7" & sl_gather$variable=="yield",]
which(test$value>0)
test$time <- paste(ifelse(test$timeUse<10,"0",""), test$timeUse, sep="")
test[test$month=="Dec",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title = "Total load of Nepal SL7 per hour in Aug 2019",
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

test[test$month=="Jan",] %>%
  ggplot(aes(x=time, y=value)) +
  geom_boxplot(aes(fill=time))  +
  labs(title = "Total load of Nepal SL4 per hour in Jan 2020",
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

sl_monthly <- sl_gather %>%
  group_by(month, streetlight, variable) %>%
  summarise(value = sum(value, na.rm=TRUE))
sl_monthly <- as.data.frame(sl_monthly)
sl_monthly$value <- sl_monthly$value / 1000.0 #kWh
test <- sl_monthly[sl_monthly$variable=="Actual_PV_power_W",]
test 

sl_monthly_spread <- spread(sl_monthly, variable, value)
sl_monthly_spread$yield <- ifelse(sl_monthly_spread$Battery_power_W<0, 
                           sl_monthly_spread$Battery_power_W, 
        sl_monthly_spread$Battery_power_W - sl_monthly_spread$PV_DC_coupled_W)


########################################################################
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl_1 <- read.csv("SL_all_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
sl_1$timestamp <- as.POSIXct(sl_1$timestamp, tz="GMT", origin="1970-01-01")
sl_1 <- sl_1[sl_1$streetlight=="SL1",]
sl_1 <- sl_1[,-c(2)]
write.csv(sl_1, file="SL1_jul_mar.csv", row.names = FALSE)

