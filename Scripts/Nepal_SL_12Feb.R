library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

############# Read in data for SL stitched by Nandor under Nepal SL/Full #############################
#Extract values System overview Battery Power, System overview PV-DC-coupled,
#Battery monitor state of charge, Solar Charger PV Power, 
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
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  
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
  systemData <- cbind(sysOverview, solarCharger$Solar.Charger.PV.power, batteryMonitor[,-1])
  systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01")
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W","State_of_charge","Discharged_energy_kWh","Charged_energy_kWh")
  
  ######Analyse data to get hourly values #######################################
  system_gather <- gather(systemData, "id", "value", 2:7)
  system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
  system_gather$date <- date(system_gather$timestamp)
  
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | system_gather$id=="PV_power_W" |
                                   system_gather$id=="Battery_power_W" | system_gather$id=="State_of_charge",] %>%
    group_by(date,timeUse,id) %>%
    summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
  
  #Calculate hourly values for discharged and charged energy by taking hourly differences
  battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
  battery_charge <- battery_charge[complete.cases(battery_charge), ]
  #Extract hourly values by taking the last value for each hour 
  battery_charge_hours <- battery_charge %>%
    group_by(date, timeUse, id) %>%
    summarise(value = max(value,na.rm = TRUE))
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
    summarise(value = max(value,na.rm = TRUE))
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
#Save the file
sl_write <- spread(sl, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:01",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
#Remove data before July 2019
sl_write <- sl_write[sl_write$date>=as.Date("2019-07-01"),]

#Replace NA values for charged and discharged energy with 0
sl_write$Charged_energy_kWh[is.na(sl_write$Charged_energy_kWh)] <- 0
sl_write$Discharged_energy_kWh[is.na(sl_write$Discharged_energy_kWh)] <- 0

sl_all <- sl_write
sl_write <- sl_write[,-c(1,2)]
sl_write <- sl_write[, c(8, 1, 2, 3, 4,5,6,7)]
colnames(sl_write) <- c("Time","ID","Battery_power_W","Charged_energy_W","Discharged_energy_W",
                        "PV_DC_coupled_W", "PV_power_W", "State_of_charge_%")
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(sl_write, file="SL_all.csv", row.names=FALSE)

############################ Read in data after the one stitched #############################
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl2 <- data.frame()
for(k in 1:7) {
  #Read files for each CPE
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
    setwd("./SL5/")
  } else if(k==6) {
    setwd("./SL6/")
  } else if(k==7) {
    setwd("./SL7/")
  }
  
  #For each CPE - read files for each month - July, Aug, Sep, Oct, Nov and Dec
  monthsList <- c("Jul","Aug","Sep","Oct","Nov","Dec")
  #Files available from 19th July 2019
  for(j in 1:length(monthsList)) {
    if(j==1) {
      setwd("./07 2019/")
    } else if(j==2) {
      setwd("../08 2019/")
    } else if(j==3) {
      setwd("../09 2019/")
    } else if(j==4) {
      setwd("../10 2019/")
    } else if(j==5) {
      setwd("../11 2019/")
    } else if(j==6) {
      setwd("../12 2019/")
    }
    
    #Input the file for each month
    file_list <- list.files()
    for(i in 1:length(file_list)) {
      #Concatenate the headers spread across first 3 rows
      headers <- read_csv(file_list[i], col_names = FALSE, na="..", n_max = 3)
      #Replace NA in header with "" for missing row 3 values
      headers[is.na(headers)] <- ""
      column_labels <- headers %>% summarize_all(str_c, collapse = " ")
      headers = unname(unlist(column_labels[1,]))
      
      #Read data without the first three rows
      df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
      #Replace NA in data frame with "" for missing values as in raw file
      df[is.na(df)] <- ""
      
      #Extract values for System overview Battery Power and System overview PV-DC-coupled
      #Find all column names with System overview
      columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
      #Find all column names with PV - DC-coupled W
      colNames <- columns[which(grepl("PV - DC-coupled W", columns, fixed=TRUE))]
      #Find all column names with Battery Power W
      colNames <- c(colNames, columns[which(grepl("Battery Power W", columns, fixed=TRUE))])
      colNames <- c(headers[1], colNames) 
      sysOverview <- df[,colNames]
      
      #Extract values for Solar Charger PV power
      #Find all column names with Solar Charger
      columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
      #Find all column names with PV power
      colNames <- columns[which(grepl("PV power", columns, fixed=TRUE))]
      colNames <- c(headers[1], colNames) 
      solarCharger <- df[,colNames]
      
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
      
      systemData <- data.frame()
      systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
      colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                                "PV_power_W","State_of_charge","Discharged_energy_kWh","Charged_energy_kWh")
      systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
      systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
      systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
      systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
      
      #Trim data to start from 1st of the month 
      systemData$month <- as.character(month(systemData$timestamp, label=TRUE, abbr=TRUE))
      systemData <- systemData[systemData$month==monthsList[j], ]
      
      ######Analyse data to get hourly values #######################################
      system_gather <- gather(systemData, "id", "value", 2:7)
      system_gather$timeUse <- format(system_gather$timestamp, format='%H')  
      system_gather$date <- date(system_gather$timestamp)
      
      #Hourly means can be calculated for AC consumption and PV power
      system_hourly <- system_gather[system_gather$id=="PV_DC-coupled_W" | system_gather$id=="PV_power_W" |
                                       system_gather$id=="Battery_power_W",] %>%
        group_by(date,timeUse,id) %>%
        summarise(value=mean(value,na.rm = TRUE))
      system_hourly <- as.data.frame(system_hourly)
      
      #Calculate the last value in an hour for state of charge
      system_soc <- system_gather[system_gather$id=="State_of_charge",] %>%
        group_by(date,timeUse,id) %>%
        summarise(value=value[length(value)])
      system_soc <- as.data.frame(system_soc)
      system_hourly <- rbind(system_hourly, system_soc)
      
      #Calculate hourly values for discharged and charged energy by taking hourly differences
      battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
      battery_charge <- battery_charge[complete.cases(battery_charge), ]
      #Extract hourly values by taking the last value for each hour 
      battery_charge_hours <- battery_charge %>%
        group_by(date, timeUse, id) %>%
        summarise(value = value[length(value)])
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
        summarise(value = value[length(value)])
      battery_discharge_hours <- as.data.frame(battery_discharge_hours)
      a <- diff(battery_discharge_hours$value)
      battery_discharge_hours <- battery_discharge_hours[-1,]
      battery_discharge_hours$value <- a
      battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
      system_hourly <- rbind(system_hourly, battery_discharge_hours)
      
      #Add the Streetlight ID to the data
      system_hourly$streetlight <- rep(paste("SL",k,sep=""), length(system_hourly$date))
      
      #Bind to sl data frame
      sl <- rbind(sl, system_hourly)
    }
  }
}


##############################################################################################
#Get missing data - per month per SL
sl_all_gather <- gather(sl_all, "variable", "value", 4:9)
sl_all_gather[is.na(sl_all_gather)] <- NA
sl_all_gather <- na.omit(sl_all_gather)
sl_all_gather$month <- as.character(month(sl_all_gather$date, label=TRUE, abbr=TRUE))
missing_sl <- sl_all_gather %>%
  group_by(month,date,streetlight,variable) %>%
  summarise(missingHours = 24-length(timeUse))
missing_sl <- as.data.frame(missing_sl)

#Plotting for SL1
missing_sl[missing_sl$streetlight=="SL1",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL1" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL2
missing_sl[missing_sl$streetlight=="SL2",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL2" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL3
missing_sl[missing_sl$streetlight=="SL3",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL3" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL4
missing_sl[missing_sl$streetlight=="SL4",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL4" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL5
missing_sl[missing_sl$streetlight=="SL5",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL5" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL6
missing_sl[missing_sl$streetlight=="SL6",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL6" , 
       y="Number of hours missing",
       x = "Date of month")

#Plotting for SL7
missing_sl[missing_sl$streetlight=="SL7",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable)) +
  geom_point() + 
  labs(title="Missing data for SL7" , 
       y="Number of hours missing",
       x = "Date of month")
