library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(stringr)
library(rlang)
library(readr)

############################ Read in data for Nepal SL3 #############################
#Extract values System overview Battery Power, System overview PV-DC-coupled,
#Battery monitor state of charge, Solar Charger PV Power, 
#Battery Monitor Discharged energy and Battery Monitor Charged Energy 

setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl <- data.frame()
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
#Save the file
sl_write <- spread(sl, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:01",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
#Check if any variable other than charged/discharged energy are NA
any(is.na(sl_write$Battery_power_W) == TRUE) #check for all
sl_write[is.na(sl_write)] <- 0
sl_all <- sl_write
sl_write <- sl_write[,-c(1,2)]
sl_write <- sl_write[, c(8, 1, 2, 3, 4,5,6,7)]
colnames(sl_write) <- c("Time","ID","Battery_power_W","Charged_energy_W","Discharged_energy_W",
                        "PV_DC_coupled_W", "PV_power_W", "State_of_charge_%")
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(sl_write, file="SL_all.csv", row.names=FALSE)

##############################################################################################
#Get missing data - per month per SL
sl_all_gather <- gather(sl_all, "variable", "value", c(4:9,13))
missing_sl <- sl_all_gather %>%
  group_by(month,date,streetlight,variable) %>%
  summarise(missingHours = 24-length(timeUse))
missing_sl <- as.data.frame(missing_sl)

#Plot for each SL missing data per month
#Plotting for SL3 in Oct 2019
missing_sl[missing_sl$month=="Oct",] %>%
  ggplot(aes(x=date, y=missingHours, color=variable, shape=streetlight)) +
  geom_point() + 
  labs(title="Missing data for Nepal SL in Oct 2019" , 
       y="Number of hours missing",
       x = "Date of month")

############################################################################################
#Hourly socket and system consumption for streetlight (1-7) in (month). 
#system and socket loads = Battery power - pv dc-coupled
sl_all$day <- weekdays(sl_all$date, abbr=TRUE)
sl_all$month <- as.character(month(sl_all$date, label=TRUE, abbr=TRUE))
sl_all$sysSocketLoad <- sl_all$Battery_power_W - sl_all$`PV_DC-coupled_W`

sl_all_weday <- sl_all[sl_all$day=="Sat" | sl_all$day=="Sun", c(1,2,3,12,13)]
sl_all_wday <- sl_all[sl_all$day!="Sat" & sl_all$day!="Sun", c(1,2,3,12,13)]

sl_all_wday[sl_all_wday$month=="Oct",] %>%
  ggplot(aes(x=timeUse, y=abs(sysSocketLoad))) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Mean hourly system and socket load at SL1 on a weekday in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

sl_all_weday[sl_all_weday$month=="Oct",] %>%
  ggplot(aes(x=timeUse, y=abs(sysSocketLoad))) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Mean hourly system and socket load at SL1 on a weekend day in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

sl_all_wday[sl_all_wday$month=="Oct",] %>%
  ggplot(aes(x=timeUse, y=abs(sysSocketLoad))) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Mean hourly system and socket load at SL3 on a weekday in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

sl_all_weday[sl_all_weday$month=="Oct",] %>%
  ggplot(aes(x=timeUse, y=abs(sysSocketLoad))) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Mean hourly system and socket load at SL3 on a weekend day in Oct'19" , 
       y="Power consumption (W)",
       x = "Hour of day") + 
  theme(plot.title = element_text(size=10), legend.position = "none") 

#Typical week (a) and weekend (b) day hourly socket and system consumption for a streetlight in (month).
typical_sl_wday <- sl_all_wday %>%
  group_by(streetlight, month, timeUse) %>%
  summarise(load = mean(sysSocketLoad))
typical_sl_wday <- as.data.frame(typical_sl_wday)

typical_sl_weday <- sl_all_weday %>%
  group_by(streetlight, month, timeUse) %>%
  summarise(load=mean(sysSocketLoad))
typical_sl_weday <- as.data.frame(typical_sl_weday)

#Typical day values for system and socket consumption of SL1 in Oct 
typical_sl_wday[typical_sl_wday$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), abs(load), color=streetlight)) + 
  geom_line(aes(linetype=streetlight)) +
  geom_point(aes(shape=streetlight)) + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical weekday system and sockets load at Nepal SL in Oct'19" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Streetlight", 
       shape="Streetlight",
       linetype="Streetlight")

typical_sl_weday[typical_sl_weday$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), abs(load), color=streetlight)) + 
  geom_line(aes(linetype=streetlight)) +
  geom_point(aes(shape=streetlight)) + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical weekend day system and sockets load at Nepal SL in Oct'19" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Streetlight", 
       shape="Streetlight",
       linetype="Streetlight")

#Typical day values for system and socket consumption of SL3 in Oct 
typical_sl_wday[typical_sl_wday$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), abs(load), color=streetlight)) + 
  geom_line(aes(linetype=streetlight)) +
  geom_point(aes(shape=streetlight)) + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical weekday system and sockets load at Nepal SL3 in Oct'19" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Streetlight", 
       shape="Streetlight",
       linetype="Streetlight")

typical_sl_weday[typical_sl_weday$month=="Oct",] %>%
  ggplot(aes(as.numeric(timeUse), abs(load), color=streetlight)) + 
  geom_line(aes(linetype=streetlight)) +
  geom_point(aes(shape=streetlight)) + 
  scale_shape_manual(values=c(0,1,4)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted")) +
  labs(title="Typical weekend day system and sockets load at Nepal SL3 in Oct'19" ,
       y="Power consumption (W)",
       x = "Time of day (hours)" ,
       color="Streetlight", 
       shape="Streetlight",
       linetype="Streetlight")
################################################################################################
###Daily socket and system consumption since commissioning
startDate <- as.Date("2019-07-01")
sl_all_daily <- sl_all %>%
  group_by(month, streetlight, date) %>%
  summarise(load = sum(sysSocketLoad))
sl_all_daily <- as.data.frame(sl_all_daily)
sl_all_daily$days <- as.numeric(sl_all_daily$date - startDate)

sl_all_daily %>%
  ggplot(aes(x=days, y=abs(load), color="Actual")) +
  geom_point(shape=8) + 
  labs(title="Daily system and sockets at SL3 load since commissioning (2019-07-01) " , 
       y="Total energy consumption (Wh)",
       x = "Days since commissioning",
       colour="") + 
  theme(plot.title = element_text(size=11),legend.position = "none") +
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140,160,180),
                     labels=c("0","20","40","60","80","100","120","140","160","180"))

#Getting daily average per month
sl_all_daily_avg <- sl_all_daily %>%
  group_by(month) %>%
  summarise(load=mean(load))
sl_all_daily_avg <- as.data.frame(sl_all_daily_avg)
sl_all_daily_avg$id <- rep("Actual", length(sl_all_daily_avg$month))
sl_all_daily_avg$month2 <- factor(sl_all_daily_avg$month, 
                              levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),
                              labels = c("Jul","Aug","Sep","Oct","Nov","Dec"))
sl_all_daily_avg$id <- as.factor(sl_all_daily_avg$id)
sl_all_daily_avg %>%
  ggplot(aes(x=month2, y=abs(load), fill=id)) +
  geom_bar(stat="identity", width=.5, position="dodge") + 
  labs(title="Average daily systems and sockets load at SL3 for each month" , 
       y="Average daily energy consumption (Wh)",
       x = "Month", 
       fill="") + 
  theme(plot.title = element_text(size=11),legend.position = "none")

############################################################################
###Calculating socket load by subtracting system load - system load would vary for each streetlight
#sl_all_daily$socketLoad <- sl_all_daily$load + (7.5*24) #for SL1
sl_all_daily$socketLoad <- sl_all_daily$load + (3.5*24) #for SL3
##Daily load for each socket in Oct
sl_all_daily[sl_all_daily$month=="Oct", ] %>%
  ggplot(aes(x = date, y= abs(socketLoad), fill = streetlight)) +
  geom_bar(stat="identity", width=.5, position = "stack") + 
  labs(title="Estimated streetlight socket consumption values in Oct'19" , 
       y="Daily socket consumption (Wh)",
       x = "Day of month",
       fill="") +
  theme(axis.text.x = element_text(size=9)) +
  scale_x_date(date_minor_breaks = "1 day")

############################################################################################
#Reading in weather data
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Weather data/")
file_list <- list.files()
for(i in 1:length(file_list)) {
  headers <- read_excel(file_list[i], col_names = FALSE, na="..", n_max = 2)
  #Replace NA in header with "" for missing row 3 values
  headers[is.na(headers)] <- ""
  column_labels <- headers %>% summarize_all(str_c, collapse = " ")
  headers = unname(unlist(column_labels[1,]))
  weather_data <- read_excel(file_list[i], col_names = headers, na="..", skip = 2,
                             col_types=c("date","numeric","numeric"))
  weather_data <- weather_data[,-2]
  weather_data$`In-plane PV Incident Solar kW/m2` <- weather_data$`In-plane PV Incident Solar kW/m2` * 
    1.968 * 0.152 * 1000.0
  s <- unlist(strsplit(gsub("_20.*","",file_list[i]), "_"))
  for(j in 1:(length(s)-1)) {
    weather_data <- cbind(weather_data, weather_data$`In-plane PV Incident Solar kW/m2`)
  }
  colnames(weather_data) <- c("timestamp",s)
  
  if(i==1) {
    weather <- weather_data
  } else {
    weather <- cbind(weather, weather_data[,-1])
  }
}
weather_data <- weather
##Data is 12 years behind - change year to 2019
weather_data$timestamp <- weather_data$timestamp %m+% years(12)
weather_data <- weather_data[weather_data$timestamp>="2019-07-01 00:00:00 GMT", ]
weather_data <- weather_data[complete.cases(weather_data), ]
weather_data <- weather_data[-1,]
weather_data <- weather_data[,c(1,2,7,8,3:6)]
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(weather_data, file="weather_hourly_jul_dec.csv", row.names=FALSE)

####### Typical day profile ##############################
#Act. PV Power Output: [PV Power] - got the hourly values for PV_power_W in sl_all
act_power_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(act_power = mean(PV_power_W))
act_power_typical <- as.data.frame(act_power_typical)

#Total load: [IF (Battery Power <0, = Battery Power), IF(Battery Power >0, = Battery Power – PV-DC-Coupled)]. 
#calculate total load values
sl_all$totalLoad <- sl_all$Battery_power_W
for(i in 1:length(sl_all$date)) {
  if(sl_all$Battery_power_W[i]>=0) {
    sl_all$totalLoad[i] = sl_all$Battery_power_W[i] - sl_all$`PV_DC-coupled_W`[i]
  }
}
load_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(totalLoad = mean(totalLoad))
load_typical <- as.data.frame(load_typical)

#Battery Charge Power - get hourly values from sl_all
battery_charge_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_charge = mean(Charged_energy_kWh))
battery_charge_typical <- as.data.frame(battery_charge_typical)

#Battery Discharge Power  - get hourly values from sl_all
battery_discharge_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_discharge = mean(Discharged_energy_kWh))
battery_discharge_typical <- as.data.frame(battery_discharge_typical)

#Battery state of charge is stated directly - get hourly values
battery_state_typical <- sl_all %>%
  group_by(month, streetlight,timeUse) %>%
  summarise(battery_state = mean(State_of_charge))
battery_state_typical <- as.data.frame(battery_state_typical)

#Hourly Pot. PV power output from weather data
weather <- gather(weather_data, "streetlight", "Pot_PV_power", 2:8)
weather$timeUse <- format(weather$timestamp, format='%H')  
weather$month <- as.character(month(weather$timestamp, label=TRUE, abbr=TRUE)) 
pot_pv_typical <- weather %>%
  group_by(month, streetlight, timeUse) %>%
  summarise(pot_pv = mean(Pot_PV_power))
pot_pv_typical <- as.data.frame(pot_pv_typical)
#Select SL3 and Oct for now
pot_pv_typical <- pot_pv_typical[pot_pv_typical$streetlight=="SL3" & pot_pv_typical$month=="Oct", ]

#Capture losses = Pot. PV power output – Act. PV power output 
pot_pv_typical$captureLosses <- pot_pv_typical$pot_pv - act_power_typical$act_power

#Getting all the typical day values together for the system
typical_day <- pot_pv_typical
typical_day <- cbind(typical_day, act_power_typical$act_power, load_typical$totalLoad,
                     battery_charge_typical$battery_charge, battery_discharge_typical$battery_discharge,
                     battery_state_typical$battery_state)
colnames(typical_day) <- c("month","streetlight","timeUse","Pot PV power","Capture loss","Actual PV power",
                           "Total load", "Battery charge energy", "Battery discharge energy",
                           "Battery state of charge")

###Plotting data for a typical day for Oct 2019 
#Plotting all variables for SL1
typical_day$timeUse <- as.numeric(typical_day$timeUse)
typical_day[typical_day$streetlight=="SL1" & typical_day$month=="Oct", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = `Pot PV power`, color = "Potential PV power", group="Potential PV power"), linetype=1) + 
  geom_point(aes(y = `Pot PV power`, color = "Potential PV power"), shape=1) + 
  geom_line(aes(y = `Actual PV power`, color = "Actual PV power", group="Actual PV power"), linetype=2) + 
  geom_point(aes(y = `Actual PV power`, color = "Actual PV power"), shape=2) + 
  geom_line(aes(y = `Capture loss`, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = `Capture loss`, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = `Battery charge energy`, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = `Battery charge energy`, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = `Battery discharge energy`, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = `Battery discharge energy`, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = abs(`Total load`), color = "Total load", group="Total load"), linetype=6) + 
  geom_point(aes(y = abs(`Total load`), color = "Total load"), shape=6) + 
  geom_line(aes(y = `Battery state of charge`*2, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = `Battery state of charge`*2, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "State of charge (%)")) +
  labs(title="Typical day profile for Nepal SL1 in Oct 2019" , 
       y="Energy (Wh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

#Plotting all variables for SL3
typical_day$timeUse <- as.numeric(typical_day$timeUse)
typical_day[typical_day$streetlight=="SL3" & typical_day$month=="Oct", ] %>%
  ggplot(aes(x=timeUse)) +
  geom_line(aes(y = `Pot PV power`, color = "Potential PV power", group="Potential PV power"), linetype=1) + 
  geom_point(aes(y = `Pot PV power`, color = "Potential PV power"), shape=1) + 
  geom_line(aes(y = `Actual PV power`, color = "Actual PV power", group="Actual PV power"), linetype=2) + 
  geom_point(aes(y = `Actual PV power`, color = "Actual PV power"), shape=2) + 
  geom_line(aes(y = `Capture loss`, color = "Capture losses", group="Capture losses"), linetype=3) + 
  geom_point(aes(y = `Capture loss`, color = "Capture losses"), shape=3) + 
  geom_line(aes(y = `Battery charge energy`, color = "Charged Energy", group="Charged Energy"), linetype=4) + 
  geom_point(aes(y = `Battery charge energy`, color = "Charged Energy"), shape=4) + 
  geom_line(aes(y = `Battery discharge energy`, color = "Discharged Energy", group="Discharged Energy"), linetype=5) + 
  geom_point(aes(y = `Battery discharge energy`, color = "Discharged Energy"), shape=5) + 
  geom_line(aes(y = abs(`Total load`), color = "Total load", group="Total load"), linetype=6) + 
  geom_point(aes(y = abs(`Total load`), color = "Total load"), shape=6) + 
  geom_line(aes(y = `Battery state of charge`*2, color = "State of charge", group="State of charge"), linetype=7) + 
  geom_point(aes(y = `Battery state of charge`*2, color = "State of charge"), shape=7) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "State of charge (%)")) +
  labs(title="Typical day profile for Nepal SL3 in Oct 2019" , 
       y="Energy (Wh)",
       x = "Time of day (00-23 hours)", 
       colour="Parameter") +
  scale_x_continuous(labels=c("00","02","04","06","08","10","12","14","16","18",
                              "20","22"),
                     breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))

##Capture losses and surplus - daily values since commissioning
#Capture loss per day is given by pot pv power - act pv power - get hourly value
#Get Pot. PV daily per SL and per day
weather$date <- date(weather$timestamp)
#For now we only get for SL3
weather_daily <- weather %>%
  group_by(month, date, streetlight) %>%
  summarise(pot_pv = sum(Pot_PV_power))
weather_daily <- as.data.frame(weather_daily)
#Extract Oct for now for SL3
weather_daily <- weather_daily[weather_daily$streetlight=="SL3" & weather_daily$month=="Oct",]

#Get Act PV daily - per SL per day
act_power_daily <- sl_all %>%
  group_by(month, date, streetlight) %>%
  summarise(act_pv = sum(PV_power_W))
act_power_daily <- as.data.frame(act_power_daily)
#Getting daily capture losses in total - all SL
weather_daily$loss <- weather_daily$pot_pv - act_power_daily$act_pv #Wh
weather_daily$days <- as.numeric(weather_daily$date - startDate)

#Getting % of surplus used by sockets and system
#Surplus = [1-captureLosses(Wh)/(daily system and socket loads (Wh) + capture losses(Wh))]
#The system and socket load is given by [System Battery Power (W) - System PV-DC-Coupled (W)]
#system and socket load combined per SL per day - sysSocketLoad
sysSocketLoad_daily <- sl_all %>%
  group_by(month, date, streetlight) %>%
  summarise(load = sum(sysSocketLoad))
sysSocketLoad_daily <- as.data.frame(sysSocketLoad_daily)
sysSocketLoad_daily$surplus <- 1 - abs(weather_daily$loss)/(abs(sysSocketLoad_daily$load) + 
                                                              abs(weather_daily$loss))
weather_daily$surplus <- sysSocketLoad_daily$surplus * 100.0
weather_daily$load <- sysSocketLoad_daily$load

weather_daily %>%
  ggplot(aes(x=days)) +
  geom_point(aes(y = loss, color = "Capture losses"), shape=1) +
  geom_point(aes(y = abs(load), color = "System and socket load"), shape=2) +
  geom_point(aes(y = abs(surplus)*15, color = "Surplus (%)"), shape=3) +
  scale_y_continuous(sec.axis = sec_axis(~./15, name = "Surplus (%)")) +
  labs(title="Estimated capture losses, load and % surplus used by external devices at Nepal SL3 in Oct'19" , 
       y="Capture losses and load (Wh)",
       x = "Number of days since commissioning", 
       colour="Parameter") +
  theme(plot.title = element_text(size=9))
