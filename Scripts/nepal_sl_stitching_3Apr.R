library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)
library(openxlsx)

### Read in data for all SL - read in stitched data till Oct and then onwards
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Full/")
file_list <- list.files()
sl <- data.frame()
for(k in 1:length(file_list)) {
  df <- read.csv(file_list[k], header=TRUE, stringsAsFactors = FALSE)
  
  headers = colnames(df)
  #Extract values for System overview Battery Power and System overview PV-DC-coupled
  columns <- headers[which(grepl("System.overview", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("PV...DC.coupled", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("Battery.Power", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  sysOverview <- df[,colNames]
  sysOverview <- as.data.frame(sysOverview)
  sysOverview <- sysOverview[,c(1:length(sysOverview))]
  
  #Extract values for Solar Charger PV current and voltage, Battery current and voltage
  #Battery watts, Load current
  columns <- headers[which(grepl("Solar.Charger", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("PV.current", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("PV.voltage", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Battery.current", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Battery.voltage", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Battery.watts", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Load.current", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  solarCharger <- as.data.frame(solarCharger)
  solarCharger <- solarCharger[,c(1:length(solarCharger))]
  
  solarCharger$Solar.Charger.PV.power <- 
    solarCharger$Solar.Charger.PV.current * solarCharger$Solar.Charger.PV.voltage
  solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and voltage
  if(length(solarCharger)==6) {
    solarCharger$Solar.Charger.Battery.watts.W <- 
      ifelse(is.na(solarCharger$Solar.Charger.Battery.watts.W),
             solarCharger$Solar.Charger.Battery.current.A * 
               solarCharger$Solar.Charger.Battery.voltage.V,
             solarCharger$Solar.Charger.Battery.watts.W)
    solarCharger <- solarCharger[,-c(2,3)] #Remove Battery current and voltage
  }
  solarCharger <- solarCharger[,c(1,4,2:3)]
  
  #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
  columns <- headers[which(grepl("Battery.Monitor", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("State.of.charge", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("Discharged.Energy", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Charged.Energy", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  batteryMonitor <- df[,colNames]
  batteryMonitor <- as.data.frame(batteryMonitor)
  batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
  
  systemData <- data.frame()
  systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  #Time is in Nepal (GMT+5:45) - just parsing time to time in GMT
  if(k!=6) {
    systemData$timestamp <- as.POSIXct(systemData$timestamp, tz="GMT", origin="1970-01-01")
  } else {
    systemData$timestamp <- as.POSIXct(systemData$timestamp,format="%d/%m/%Y %H:%M", tz="GMT")
  }
  
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W", "Battery_watts_W", "Load_current_A","State_of_charge",
                            "Discharged_energy_kWh","Charged_energy_kWh")
  
  #Add the Streetlight ID to the data
  systemData$streetlight <- rep(gsub("_.*","",file_list[k]), length(systemData$timestamp))
  
  #Bind to sl data frame
  sl <- rbind(sl, systemData)
}
write.csv(sl, "~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_jul_oct.csv",
          row.names = FALSE)

############################ Read in data after the one stitched #############################
#For SL1 - Nov, Dec, Jan and Feb directories ; For SL2, SL4, SL7 - Feb directories
#For SL3 and SL6 - Dec and Feb directories ; For SL5 - no files atm 
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
    } else {
      #Read data without the first three rows
      df <- read_csv(file_list[i], col_names = headers, na="..", skip = 3)
    }
    #Replace NA in data frame with "" for missing values as in raw file
    df[is.na(df)] <- ""
    
    #Extract values for System overview Battery Power and System overview PV-DC-coupled
    columns <- headers[which(grepl("System overview", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV - DC-coupled", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Battery Power", columns, fixed=TRUE))])
    colNames <- c(headers[1], colNames) 
    sysOverview <- df[,colNames]
    sysOverview <- as.data.frame(sysOverview)
    sysOverview <- sysOverview[,c(1:length(sysOverview))]
    
    #Extract values for Solar Charger PV current and voltage, battery current and voltage,
    #Battery watts and Load current
    columns <- headers[which(grepl("Solar Charger", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("PV current", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("PV voltage", columns, fixed=TRUE))])
    colNames <- c(colNames, columns[which(grepl("Load current", columns, fixed=TRUE))])
    if(grepl("SD", name, fixed=TRUE)) {
      colNames <- c(colNames, columns[which(grepl("Battery current", columns, fixed=TRUE))])
      colNames <- c(colNames, columns[which(grepl("Battery voltage", columns, fixed=TRUE))])
    } else {
      colNames <- c(colNames, columns[which(grepl("Battery watts", columns, fixed=TRUE))])
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
      solarCharger <- solarCharger[,-c(5,6)] #Remove Battery current and battery voltage
    } else {
      colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                                  "Battery_watts_W")
    }
    solarCharger$PV_power_W <- 
      solarCharger$PV_current_A * solarCharger$PV_voltage_V
    solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
    solarCharger <- solarCharger[,c(1,4,3,2)] #Rearrange columns
    
    #Extract values for Battery Monitor State of charge %, Discharged Energy kWh, Charged Energy kWh
    columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
    colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
    colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
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
                              "PV_power_W", "Battery_watts_W", "Load_current_A",
                              "State_of_charge","Discharged_energy_kWh",
                              "Charged_energy_kWh")
    systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
    systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
    systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
    systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
    
    #Add the Streetlight ID to the data
    systemData$streetlight <- rep(substr(name,6,8), length(systemData$timestamp))
    
    #Bind to sl data frame
    sl2 <- rbind(sl2, systemData)
  }
}
write.csv(sl2, "~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_oct_mar.csv",
          row.names = FALSE)

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
  colNames <- c(headers[1], colNames) 
  solarCharger <- df[,colNames]
  solarCharger <- as.data.frame(solarCharger)
  solarCharger <- solarCharger[,c(1:length(solarCharger))]
  colnames(solarCharger) <- c("timestamp","PV_current_A", "PV_voltage_V", "Load_current_A",
                              "Battery_watts_W")
  solarCharger$PV_power_W <- 
    solarCharger$PV_current_A * solarCharger$PV_voltage_V
  solarCharger <- solarCharger[,-c(2,3)] #Remove PV current and PV voltage
  solarCharger <- solarCharger[,c(1,4,3,2)] #Rearrange columns
  
  columns <- headers[which(grepl("Battery Monitor", headers, fixed=TRUE))]
  colNames <- columns[which(grepl("State of charge", columns, fixed=TRUE))]
  colNames <- c(colNames, columns[which(grepl("Discharged Energy", columns, fixed=TRUE))])
  colNames <- c(colNames, columns[which(grepl("Charged Energy", columns, fixed=TRUE))])
  colNames <- c(headers[1], colNames) 
  batteryMonitor <- df[,colNames]
  batteryMonitor <- as.data.frame(batteryMonitor)
  batteryMonitor <- batteryMonitor[,c(1:length(batteryMonitor))]
  
  systemData <- data.frame()
  systemData <- cbind(sysOverview, solarCharger[,-1], batteryMonitor[,-1])
  colnames(systemData) <- c("timestamp","PV_DC-coupled_W","Battery_power_W",
                            "PV_power_W", "Battery_watts_W", "Load_current_A",
                            "State_of_charge","Discharged_energy_kWh",
                            "Charged_energy_kWh")
  
  systemData$`PV_DC-coupled_W` <- as.numeric(systemData$`PV_DC-coupled_W`)
  systemData$Battery_power_W <- as.numeric(systemData$Battery_power_W)
  systemData$Discharged_energy_kWh <- as.numeric(systemData$Discharged_energy_kWh)
  systemData$Charged_energy_kWh <- as.numeric(systemData$Charged_energy_kWh)
  systemData$State_of_charge <- as.numeric(systemData$State_of_charge)
  
  #Add the Streetlight ID to the data
  systemData$streetlight <- rep(substr(file_list[k],6,8), length(systemData$timestamp))
  
  #Bind to sl data frame
  sl3 <- rbind(sl3, systemData)
}
write.csv(sl3, "~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_rem_mar.csv",
          row.names = FALSE)

#######################################################################################################
###Read in the raw data files
sl <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_jul_oct.csv",
               header = TRUE, stringsAsFactors = FALSE)
sl$timestamp <- as.POSIXct(sl$timestamp, tz="GMT", origin="1970-01-01")

sl2 <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_oct_mar.csv",
                header = TRUE, stringsAsFactors = FALSE)
sl2$timestamp <- as.POSIXct(sl2$timestamp, tz="GMT", origin="1970-01-01")

sl3 <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw_rem_mar.csv",
                header = TRUE, stringsAsFactors = FALSE)
sl3$timestamp <- as.POSIXct(sl3$timestamp, tz="GMT", origin="1970-01-01")

sl_all <- data.frame()
sl_all <- rbind(sl_all, sl, sl2, sl3)
sl_all <- sl_all[order(sl_all$timestamp),]
sl_all$date <- date(sl_all$timestamp)
#Select dates greater than 1st July
sl_all <- sl_all[sl_all$date>="2019-07-01",]
sl_all <- sl_all[,-11]
sl_all <- sl_all[,c(1,10,2:9)]
write.csv(sl_all, "~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw.csv",
          row.names = FALSE)

################################################################################################
####################### Pre-processing to see what load values to use #############################
sl_all <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_all_raw.csv",
                   header = TRUE, stringsAsFactors = FALSE)
sl_all$timestamp <- as.POSIXct(sl_all$timestamp, tz="GMT", origin="1970-01-01")

#Remove duplicated rows
sl_all <- distinct(sl_all)

#System and socket load
sl_all$sysSocketLoad <- sl_all$Battery_watts_W - sl_all$Battery_power_W 
sl_all$socketLoad <- sl_all$PV_DC.coupled_W - sl_all$Battery_power_W 

#Add date, month and time
sl_all$date <- date(sl_all$timestamp)
sl_all$month <- as.character(month(sl_all$timestamp, label=TRUE, abbr=TRUE))
sl_all$timeUse <- format(sl_all$timestamp, format="%H")

##Subset sl_all to contain non NA values for sysSocketLoad 
socket_all <- sl_all[!(is.na(sl_all$socketLoad)),] #calculating socket load using PV-DC
sys_all <- sl_all[!(is.na(sl_all$sysSocketLoad)),] #calcultaing socket load using Battery W

##Remove -ve values
socket_clean <- socket_all[socket_all$socketLoad>=0,]
sys_clean <- sys_all[sys_all$sysSocketLoad>=0,]

###### Calculate the 95% lower and higher indices for sysSocketLoad ##########
calcCI <- function(sl, p) {
  sl_sub <- sl %>%
    group_by(streetlight, month, timeUse) %>%
    summarise(lo = mean(sysSocketLoad, na.rm=TRUE) - p * (sd(sysSocketLoad, na.rm = TRUE)),
              hi = mean(sysSocketLoad, na.rm=TRUE) + p * (sd(sysSocketLoad, na.rm=TRUE)))
  sl_sub <- as.data.frame(sl_sub)
  sl_sub <- sl_sub[complete.cases(sl_sub),]
}
sl_clean_95 <- calcCI(sys_clean, 2)

###### Calculate the 95% lower and higher indices for socketLoad ##########
socket_calcCI <- function(sl, p) {
  sl_sub <- sl %>%
    group_by(streetlight, month, timeUse) %>%
    summarise(lo = mean(socketLoad, na.rm=TRUE) - p * (sd(socketLoad, na.rm = TRUE)),
              hi = mean(socketLoad, na.rm=TRUE) + p * (sd(socketLoad, na.rm=TRUE)))
  sl_sub <- as.data.frame(sl_sub)
  sl_sub <- sl_sub[complete.cases(sl_sub),]
}
socket_clean_95 <- socket_calcCI(socket_clean, 2)

####### Apply the 95% rule to subset the data - sysSocketLoad ################
sl_all_clean_95 <- data.frame()
for(i in 1:length(unique(sl_clean_95$streetlight))) {
  df <- sys_clean[sys_clean$streetlight == unique(sl_clean_95$streetlight)[i], ]
  df_95 <- sl_clean_95[sl_clean_95$streetlight == unique(sl_clean_95$streetlight)[i], ]
  for(j in 1:length(unique(df_95$month))) {
    df_sub <- df[df$month == unique(df_95$month)[j], ]
    df_sub_95 <- df_95[df_95$month == unique(df_95$month)[j], ]
    for(k in 1:length(df_sub_95$timeUse)) {
      df1 <- df_sub[df_sub$timeUse==df_sub_95$timeUse[k], ]
      df1 <- df1[df1$sysSocketLoad>=df_sub_95$lo[k] & df1$sysSocketLoad<=df_sub_95$hi[k],]
      df1 <- df1[!is.na(df1$sysSocketLoad),]
      sl_all_clean_95 <- rbind(sl_all_clean_95, df1)
    }
  }
}

####### Apply the 95% rule to subset the data - socketLoad ################
socket_all_clean_95 <- data.frame()
for(i in 1:length(unique(socket_clean_95$streetlight))) {
  df <- socket_clean[socket_clean$streetlight == unique(socket_clean_95$streetlight)[i], ]
  df_95 <- socket_clean_95[socket_clean_95$streetlight == unique(socket_clean_95$streetlight)[i], ]
  for(j in 1:length(unique(df_95$month))) {
    df_sub <- df[df$month == unique(df_95$month)[j], ]
    df_sub_95 <- df_95[df_95$month == unique(df_95$month)[j], ]
    for(k in 1:length(df_sub_95$timeUse)) {
      df1 <- df_sub[df_sub$timeUse==df_sub_95$timeUse[k], ]
      df1 <- df1[df1$sysSocketLoad>=df_sub_95$lo[k] & df1$sysSocketLoad<=df_sub_95$hi[k],]
      df1 <- df1[!is.na(df1$socketLoad),]
      socket_all_clean_95 <- rbind(socket_all_clean_95, df1)
    }
  }
}

#################################### Plotting raw data ##################################
plotRawLoad <- function(df) {
  df %>%
    ggplot(aes(timeUse, sysSocketLoad, color=month)) +
    geom_point() +
    labs(x="Time of day (00-23 hours)",
         y= "System and socket load")
}

plotLoad <- function(df) {
  df %>%
    ggplot(aes(timeUse, socketLoad, color=month)) +
    geom_point() +
    labs(x="Time of day (00-23 hours)",
         y= "System and socket load")
}

############################### Raw load plots ################################################
#SL1: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL2: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb; use sysSocketLoad for Mar
#SL3: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL4: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb and Mar
#SL5: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb and Mar - only Jul and Aug data
#SL6: use socketLoad for Jul, Aug, Sep, Oct; use sysSocketLoad for Jan, Feb, Mar - skip Nov and Dec
#SL7: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb and Mar

plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL7" & socket_all_clean_95$month=="Nov", ]) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL7")
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL7" & sl_all_clean_95$month=="Nov", ]) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL7")


#################################################################################################
#### Stitching weather data ##################################
#Reading in weather data
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Weather data/")
file_list <- list.files()
weather <- data.frame()
for(i in 1:length(file_list)) {
  headers <- read_excel(file_list[i], col_names = FALSE, na="..", n_max = 2)
  #Replace NA in header with "" for missing row 3 values
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  weather_data <- read_excel(file_list[i], col_names = headers, na="..", skip = 2,
                             col_types=c("date","date","numeric","numeric"))
  weather_data <- weather_data[,-c(1,3)]
  weather_data$`In-plane PV Incident Solar kW/m2` <- weather_data$`In-plane PV Incident Solar kW/m2` * 
    1.968 * 0.152 * 1000.0
  s <- unlist(strsplit(gsub("_20.*","",file_list[i]), "_"))
  for(j in 1:(length(s)-1)) {
    weather_data <- cbind(weather_data, weather_data$`In-plane PV Incident Solar kW/m2`)
  }
  colnames(weather_data) <- c("timestamp",s)
  weather_data <- gather(weather_data, "id", "value", 2:length(weather_data))
  
  weather <- rbind(weather, weather_data)
}
weather_data <- spread(weather, id, value)
weather_data <- weather_data[complete.cases(weather_data), ]
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(weather_data, file="weather_hourly_jul_mar.csv", row.names=FALSE)
