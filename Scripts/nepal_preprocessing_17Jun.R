#******************************************************************************************#
# This is the script for pre-processing data from all Nepal SL to convert to hourly data   #
# analyse yield of hourly data and explore imputation techniques                           #
# Author: K Bhargava                                                                       #
# Last updated on: 17th June 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(mice) # for mice imputation
library(wesanderson)
library(timeSeries) # for converting data frame into time series
library("ggpubr")
library("imputeTS") # for na_seadec imputation
library(mgcv) # for gam model-based imputation
library(simputation) # For impute_knn, impute_rf
library(randomForest) #For random Forest functions used in the back end by impute_rf
library(missForest)
library(xts)
library(MLmetrics)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read SL and weather data between July 2019 to March 2020
sl_nepal <- read_csv(here(filepath,"sl_all_raw.csv"), col_names = TRUE)
sl_nepal <- as.data.frame(sl_nepal[,seq_along(sl_nepal)])
sl_nepal <- sl_nepal %>% mutate(month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                            timeUse = hour(timestamp),Charged.energy.W = ifelse(System.overview.Battery.Power.W<0, 0, 
                                                                                System.overview.Battery.Power.W),
                            Discharged.energy.W = ifelse(System.overview.Battery.Power.W>0,0,
                                                         System.overview.Battery.Power.W)) 

weather_data <- read_csv(here(filepath,"weather_hourly_jul_mar.csv"), col_names = TRUE)
weather_data <- weather_data[,1:8]
weather_data <- gather(weather_data, "streetlight", "Potential_PV_power_W", 2:8)
weather_data$date <- date(weather_data$timestamp)
weather_data$timeUse <- hour(weather_data$timestamp)
weather_data <- weather_data[,-1] # Remove timestamp
weather_data <- weather_data[,c(1,3,4,2)] # Rearrange columns
#******************************************************************************************#

#******************************************************************************************#
# Plot number of values per hour for each day
sl_all <- gather(sl_nepal, "id", "value", c(3:8,12:13))
sl_qual <- sl_all %>% group_by(streetlight, date, timeUse, id) %>% summarise(count = length(na.omit(value)))
sl_qual <- as.data.frame(sl_qual)
sl_qual <- sl_qual %>% mutate(count2 = ifelse(count<100, count, 100))

# Calculate hourly yield for each day and SL if 60 readings expected every hour
pal <- wes_palette("Zissou1", 400, type = "continuous")
ggplot(sl_qual[sl_qual$streetlight%in%unique(sl_qual$streetlight)[1:4] & sl_qual$id=="System.overview.Battery.Power.W",],
       aes(date, timeUse)) + facet_wrap(~streetlight) + geom_tile(aes(fill = count2)) + 
  scale_fill_gradientn(colours = pal, breaks=c(0,25,50,75,100), labels=c("0","25","50","75","100+")) + 
  scale_y_continuous(breaks=seq(0,24,by=2)) + xlab("X axis") + ylab("Y axis") + 
  labs(title="Number of readings per hour for Nepal SL: 1 Jul'19 - 19 Mar'20",y="Time of day",x = "Day of study",
       fill="Readings/hour")
ggsave(here(plot_dir,"numReads_hourly1.png"))
ggplot(sl_qual[sl_qual$streetlight%in%unique(sl_qual$streetlight)[5:7] & sl_qual$id=="System.overview.Battery.Power.W",],
       aes(date, timeUse)) + facet_wrap(~streetlight, nrow=2) + geom_tile(aes(fill = count2)) + 
  scale_fill_gradientn(colours = pal, breaks=c(0,25,50,75,100), labels=c("0","25","50","75","100+")) + 
  scale_y_continuous(breaks=seq(0,24,by=2)) + xlab("X axis") + ylab("Y axis") + 
  labs(title="Number of readings per hour for Nepal SL: 1 Jul'19 - 19 Mar'20",y="Time of day",x = "Day of study",
       fill="Readings/hour")
ggsave(here(plot_dir,"numReads_hourly2.png"))
#******************************************************************************************#

#******************************************************************************************#
# Convert data into hourly mean, fill in NA for all days missing, add Potential PV data and save
system_hourly <- sl_all %>% group_by(streetlight, date, timeUse, id) %>% summarise(value = mean(value, na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)
system_hourly <- spread(system_hourly, id, value)
system_hourly[is.na(system_hourly)] <- NA

# For each streetlight see what check for missing data - 1 July 2019 to 19 March 2020 = 263 days
all_days <- seq(as.Date("2019-07-01"), as.Date("2020-03-19"), by="days")
all_hours <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1),by = "1 hour"), "%H", tz="GMT")
all_hours <- as.numeric(all_hours[-25])

# Create a complete data set for all days - add NA for all missing values
data <- data.frame()
for(i in seq_along(all_days)) {
  data <- rbind(data, data.frame(date = rep(all_days[i], length(all_hours)), timeUse=all_hours))
}
data <- data %>% mutate(sl1 = "SL1", sl2 = "SL2", sl3 = "SL3", sl4 = "SL4", sl5="SL5", sl6="SL6", sl7="SL7")
data <- gather(data, "id", "streetlight", 3:9)
data <- data[,-3] #remove column id
data <- data[,c(3,1,2)] # rearrange columns
data <- data %>% mutate(Battery.Monitor.State.of.charge..= NA, Charged.energy.W = NA, Discharged.energy.W = NA, 
                        Solar.Charger.Battery.watts.W=NA, Solar.Charger.Load.current.A=NA, Solar.Charger.PV.power.W=NA, 
                        System.overview.Battery.Power.W=NA, System.overview.PV...DC.coupled.W=NA)

# Include rows from 'data' to get all days not there in system_hourly
system_hourly <- system_hourly %>% mutate(id = paste(streetlight,date,timeUse, sep=" ")) 
data <- data %>% mutate(id=paste(streetlight,date,timeUse, sep=" "))
system_hourly <- rbind(system_hourly, data[!(data$id %in% system_hourly$id),])
system_hourly <- system_hourly[order(system_hourly$streetlight,system_hourly$date,system_hourly$timeUse),]

# Add in weather data - weather data unavailable for April
weather_data <- weather_data %>% mutate(id=paste(streetlight,date,timeUse, sep=" "))
system_hourly <- merge(system_hourly, weather_data[,-c(1:3)], by="id")
system_hourly <- system_hourly[,-1] # Remove id
system_hourly <- system_hourly[order(system_hourly$streetlight,system_hourly$date,system_hourly$timeUse),]
write.csv(system_hourly, file=here(filepath,"raw_hourly_sl_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data and calculate yield
system_hourly <- read.csv(here(filepath,"raw_hourly_sl_data.csv"), header = TRUE, stringsAsFactors=FALSE)
system_hourly <- system_hourly %>% mutate(date = as.Date(date))

# Get summary of the data to get number of NA values for each var
summary(system_hourly) # ~ 9K values missing for all variables 
# Get percentage missing data for each variable for each SL
system <- gather(system_hourly,"id","value",4:12)
missingData <- system %>% group_by(streetlight, id) %>% summarise(missingPercent = sum(is.na(value))*100/length(value))  
missingData <- spread(missingData, id, missingPercent)
write.csv(missingData, file=here(filepath,"missing_sl_data.csv"), row.names=FALSE)

# Calculate yield as % hours on during a day
sl_on_hours <- system %>% group_by(streetlight, date, id) %>% summarise(yield = length(na.omit(value)) * 100.0 / 24.0 )
sl_on_hours <- as.data.frame(sl_on_hours)

pal <- wes_palette("Zissou1", 100, type = "continuous")
ggplot(sl_on_hours[sl_on_hours$id=="System.overview.Battery.Power.W",], aes(date, streetlight)) + 
  geom_tile(aes(fill = yield)) + scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield for Nepal SL hourly data: 1 Jul'19 - 19 Mar'20",y="Streetlight",x = "Day of study",fill="Yield")
ggsave(here(plot_dir,"yield_all.png"))

ggplot(sl_on_hours, aes(date, id)) + facet_wrap(~streetlight) +  geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + xlab("X axis") + ylab("Y axis") +
  labs(title="Yield for Nepal SL hourly data: 1 Jul'19 - 19 Mar'20", y="Variable",x ="Day of study",fill="Yield") 
ggsave(here(plot_dir,"yield_sl_all.png"))
#******************************************************************************************#

#******************************************************************************************#
# Get rows where NA values are a certain proportion of variables
system_hourly$na_count <- apply(system_hourly, 1, function(x) sum(is.na(x)))
# How many have 1 or more missing variables
sum(system_hourly$na_count>0) #9215*100/44184 = 20% rows
#******************************************************************************************#

#******************************************************************************************#
# Get days for which more than 50% data is missing i.e. yield is less than 50%
sl_all_50 <- sl_on_hours[sl_on_hours$id=="System.overview.Battery.Power.W" & sl_on_hours$yield<50,] 
sl_all_50 <- sl_all_50 %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)))
# Count the number of days per SL - Sl3: 83 days, SL5:206 days, SL6 and SL7:41days  - see if these need to be removed
sl_all_50_count <- sl_all_50 %>% group_by(streetlight) %>% summarise(count=length(yield))
# Count days with more than 50% data missing per month
sl_all_50_monthly <- sl_all_50 %>% group_by(streetlight, month) %>% summarise(count=length(yield))

# Get full days per month
sl_all_100 <- sl_on_hours[sl_on_hours$id=="System.overview.Battery.Power.W" & sl_on_hours$yield==100,] 
sl_all_100 <- sl_all_100 %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)))
# Count full days per SL for each month
sl_all_100_count <- sl_all_100 %>% group_by(streetlight,month) %>% summarise(count=length(yield))
#******************************************************************************************#

#******************************************************************************************#
# Get typical hourly data and see how it varies over time
system_typical <- system %>% group_by(streetlight, timeUse, id) %>% summarise(value = mean(value, na.rm=TRUE))
system_typical <- as.data.frame(system_typical)
system_typical <- spread(system_typical, id, value)

# Plot typical values for each SL
plotTypical <- function(df) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=Charged.energy.W/1000.0, color="Charged.energy.W"),linetype=1) +
    geom_line(aes(y=abs(Discharged.energy.W)/1000.0, color="Discharged.energy.W"),linetype=2) + 
    geom_line(aes(y=Solar.Charger.PV.power.W/1000.0, color="Solar.Charger.PV.power.W"),linetype=3) +
    geom_line(aes(y=Potential_PV_power_W/1000.0, color="Potential_PV_power_W"),linetype=4) +
    labs(y="Energy (kWh)", x = "Time of day", colour="Parameter") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + theme(plot.title = element_text(size=10), legend.position = "bottom",
    legend.box = "horizontal",  legend.key.size = unit(0.5, "cm")) +
    scale_linetype_discrete(labels=c("SoC","Charged energy","Discharged energy","Potential PV power","Actual PV power"))+
    scale_color_discrete(labels=c("SoC","Charged energy","Discharged energy","Potential PV power","Actual PV power"))
}
plotTypical(system_typical[system_typical$streetlight=="SL1",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../400, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "SoC (%)")) +
  labs(title="Actual Nepal SL1 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl1.png"))

plotTypical(system_typical[system_typical$streetlight=="SL2",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../400, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "SoC (%)")) +
  labs(title="Actual Nepal SL2 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl2.png"))

plotTypical(system_typical[system_typical$streetlight=="SL3",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../455, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*455, name = "SoC (%)")) +
  labs(title="Actual Nepal SL3 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl3.png"))

plotTypical(system_typical[system_typical$streetlight=="SL4",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../400, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*400, name = "SoC (%)")) +
  labs(title="Actual Nepal SL4 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl4.png"))

plotTypical(system_typical[system_typical$streetlight=="SL5",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../500, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SoC (%)")) +
  labs(title="Actual Nepal SL5 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl5.png"))

plotTypical(system_typical[system_typical$streetlight=="SL6",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../500, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SoC (%)")) +
  labs(title="Actual Nepal SL6 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl6.png"))

plotTypical(system_typical[system_typical$streetlight=="SL7",]) + 
  geom_line(aes(y = Battery.Monitor.State.of.charge../500, color = "Battery.Monitor.State.of.charge.."), linetype=5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*500, name = "SoC (%)")) +
  labs(title="Actual Nepal SL7 power profile for a typical day from 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"typical_day_sl7.png"))
#******************************************************************************************#

#******************************************************************************************#
# Plot hourly data to see trend and seasonality - seasonality in data is 1 day as expected
# Add timestamp and month to the data
system <- system %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)),
                            timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                                      paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
                            month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                            labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Plot hourly values against time
plotHourly <- function(df) {
  ggplot(df[(df$id=="Solar.Charger.PV.power.W"|df$id=="Potential_PV_power_W"|df$id=="System.overview.Battery.Power.W"),], 
         aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
    geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
    labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable")
}
plotHourly(system[system$streetlight=="SL1",]) + 
  labs(title = "Energy profile of SL1 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl1.png"))

plotHourly(system[system$streetlight=="SL2",]) + 
  labs(title = "Energy profile of SL2 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl2.png"))

plotHourly(system[system$streetlight=="SL3",]) + 
  labs(title = "Energy profile of SL3 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl3.png"))

plotHourly(system[system$streetlight=="SL4",]) + 
  labs(title = "Energy profile of SL4 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl4.png"))

plotHourly(system[system$streetlight=="SL5",]) + 
  labs(title = "Energy profile of SL5 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl5.png"))

plotHourly(system[system$streetlight=="SL6",]) + 
  labs(title = "Energy profile of SL6 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl6.png"))

plotHourly(system[system$streetlight=="SL7",]) + 
  labs(title = "Energy profile of SL7 in Nepal: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl7.png"))
#******************************************************************************************#

#******************************************************************************************#
# Box plots 
ggplot(system[system$id=="Solar.Charger.PV.power.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "PV power at Nepal SL: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"pv_power.png"))

ggplot(system[system$id=="Charged.energy.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "Charged energy at Nepal SL: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"charged_energy.png"))

ggplot(system[system$id=="Discharged.energy.W",], aes(as.factor(timeUse), abs(value))) + 
  facet_wrap(~streetlight) + geom_boxplot() + 
  labs(x="Time of day", y="Energy (Wh)", title = "Discharged energy at Nepal SL: 01 Jul'19 to 19 Mar'20")
ggsave(here(plot_dir,"discharged_energy.png"))
#******************************************************************************************#

#******************************************************************************************#
# Get full data days for all SL
df <- system_hourly[,c(1,2,3,5,10,11,12,13,14,16,17)]
df <- df[df$na_count==0,-11]
sl_on_hours <- df %>% group_by(streetlight, date) %>% summarise(onHours = length(PV.power.W))
sl_on_hours <- as.data.frame(sl_on_hours[sl_on_hours$onHours==24,])
# Consider all full days
full_data <- data.frame()
for(i in seq_along(unique(sl_on_hours$streetlight))) {
  x <- df[df$streetlight == unique(sl_on_hours$streetlight)[i],]
  days <- sl_on_hours$date[sl_on_hours$streetlight == unique(sl_on_hours$streetlight)[i]]
  x <- x[x$date %in% days,]
  full_data <- rbind(full_data, x)
}

# Create a copy of full data and replace 20% data with NA - in system hourly we have 13% data missing for all
# variables except 24% for AC consumption
incomplete_data <- data.frame()
for(i in seq_along(unique(full_data$streetlight))) {
  df_sub <- full_data[full_data$streetlight == unique(full_data$streetlight)[i],]
  n <- length(df_sub$streetlight)
  ind <- sample( c(1:n), floor(n/5))
  df_sub[ind,c("Battery.Monitor.Voltage.V","PV.power.W","Solar.Charger.Battery.watts.W","Solar.Charger.Load.current.A",
               "System.overview.AC.Consumption.L1.W","System.overview.Battery.Power.W")] <- NA
  incomplete_data <- rbind(incomplete_data, df_sub)
}
#******************************************************************************************#

# Try different imputation techniques and compare performance using RMSE and MAPE metrics
