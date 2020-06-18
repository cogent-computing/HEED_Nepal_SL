#******************************************************************************************#
# This is the script for analysis of data for all Nepal SL data                            #
# Author: K Bhargava                                                                       #
# Last updated on: 18th Jun 2020                                                           #
#******************************************************************************************#

#****************************************************************************************************#
# Import packages
library(tidyverse)
library(lubridate)
library(wesanderson)
library(here)
#**********************************************************************************************#

#******************************************************************************************#
# Set working directory - to later read hourly imputed and corrected data
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data - read imputed data later
system_hourly <- read_csv(here(filepath,"raw_hourly_sl_data.csv"), col_names = TRUE)
system_hourly <- as.data.frame(system_hourly[,seq_along(system_hourly)])


#################################################################################################
########### Cleaning data and calculating system and socket load and light load #################
#Light load
sl_all$Light_load_W <- ifelse(sl_all$Battery_watts_W<0, sl_all$Battery_watts_W, 0)

#System and socket load
sl_all$sysSocketLoad <- sl_all$Battery_watts_W - sl_all$Battery_power_W 
sl_all$socketLoad <- sl_all$PV_DC.coupled_W - sl_all$Battery_power_W 

#Add date, month and time
sl_all$date <- date(sl_all$timestamp)
sl_all$month <- as.character(month(sl_all$timestamp, label=TRUE, abbr=TRUE))
sl_all$timeUse <- hour(sl_all$timestamp)

#SL1: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL2: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb; use sysSocketLoad for Mar
#SL3: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL4: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb and Mar
#SL5: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar - only Jul and Aug data
#SL6: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb, Mar - skip Nov and Dec
#SL7: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar

sl_new <- data.frame()
for(i in 1:length(unique(sl_all$streetlight))) {
  df <- sl_all[sl_all$streetlight==unique(sl_all$streetlight)[i], ]
  for(j in 1:length(unique(df$month))) {
    df_sub <- df[df$month==unique(df$month)[j], ]
    if(df_sub$month[1]=="Jul" | df_sub$month[1]=="Aug" | df_sub$month[1]=="Sep" |
       df_sub$month[1]=="Oct" | df_sub$month[1]=="Nov") {
      df_sub$load <- df_sub$socketLoad
    } else if((df_sub$streetlight[1]=="SL1" | df_sub$streetlight[1]=="SL3" | 
               df_sub$streetlight[1]=="SL5" | df_sub$streetlight[1]=="SL7") & 
              (df_sub$month[1]=="Dec" | df_sub$month[1]=="Jan" | 
                                                df_sub$month[1]=="Feb" | df_sub$month[1]=="Mar")) {
      df_sub$load <- df_sub$sysSocketLoad
    } else if(df_sub$streetlight[1]=="SL2" & (df_sub$month[1]=="Dec" | df_sub$month[1]=="Jan" | 
                                           df_sub$month[1]=="Feb")) {
      df_sub$load <- df_sub$socketLoad
    } else if(df_sub$streetlight[1]=="SL2" & (df_sub$month[1]=="Mar")) {
      df_sub$load <- df_sub$sysSocketLoad
    } else if((df_sub$streetlight[1]=="SL4" | df_sub$streetlight[1]=="SL6") & 
               (df_sub$month[1]=="Jan" | df_sub$month[1]=="Feb" | df_sub$month[1]=="Mar")) {
      df_sub$load <- df_sub$sysSocketLoad
    } else if((df_sub$streetlight[1]=="SL4" | df_sub$streetlight[1]=="SL6") & 
              (df_sub$month[1]=="Dec")) {
      df_sub$load <- df_sub$socketLoad
    }
    sl_new <- rbind(sl_new, df_sub)
  }
}

##Remove NA values
sl_new <- sl_new[!is.na(sl_new$load),]

##Remove load values that are negative
sl_new <- sl_new[sl_new$load>=0,]
                       
###### Calculate the 95% lower and higher indices for load - which is system and socket load #######
calcCI <- function(sl, p) {
  sl_sub <- sl %>%
    group_by(streetlight, month, timeUse) %>%
    summarise(lo = mean(load, na.rm=TRUE) - p * (sd(load, na.rm = TRUE)),
              hi = mean(load, na.rm=TRUE) + p * (sd(load, na.rm=TRUE)))
  sl_sub <- as.data.frame(sl_sub)
  sl_sub <- sl_sub[complete.cases(sl_sub),]
}
sl_95 <- calcCI(sl_new, 2)

####### Apply the 95% rule to subset the data ################
sl_all_clean_95 <- data.frame()
for(i in 1:length(unique(sl_95$streetlight))) {
  df <- sl_new[sl_new$streetlight == unique(sl_95$streetlight)[i], ]
  df_95 <- sl_95[sl_95$streetlight == unique(sl_95$streetlight)[i], ]
  for(j in 1:length(unique(df_95$month))) {
    df_sub <- df[df$month == unique(df_95$month)[j], ]
    df_sub_95 <- df_95[df_95$month == unique(df_95$month)[j], ]
    for(k in 1:length(df_sub_95$timeUse)) {
      df1 <- df_sub[df_sub$timeUse==df_sub_95$timeUse[k], ]
      df1 <- df1[df1$load>=df_sub_95$lo[k] & df1$load<=df_sub_95$hi[k],]
      df1 <- df1[!is.na(df1$load),]
      sl_all_clean_95 <- rbind(sl_all_clean_95, df1)
    }
  }
}

sl_write <- sl_all_clean_95 #Removed 4.7% of orginal data
sl_write <- sl_write[,-c(12:16)]
colnames(sl_write) <- c("timestamp", "streetlight", "PV_DC-coupled_W","Battery_power_W",
                        "PV_power_W","Battery_watts_W","Load_current_A","State_of_charge",
                        "Discharged_energy_W","Charged_energy_W","Light_load_W","Socket_load_W")
write.csv(sl_write, "~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_new_raw_jul_mar.csv",
          row.names=FALSE)

#################################################################################################
## Read in pre-processed data for analysis
sl_write <- read.csv("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/sl_new_raw_jul_mar.csv",
          header=TRUE, stringsAsFactors = FALSE)
sl_write$timestamp <- as.POSIXct(sl_write$timestamp, tz="GMT", origin="1970-01-01")

## Plots box plots #########
#sl_write - removed duplicated, NA, 0, and <95% values
#Gather data to get hourly means
sl_new <- sl_write
sl_new$date <- date(sl_new$timestamp)
sl_new$month <- as.character(month(sl_new$timestamp, label=TRUE, abbr=TRUE))
sl_new$timeUse <- hour(sl_new$timestamp)
sl_new <- gather(sl_new, "id", "value", c(3:12)) 

#Convert data to hourly means
hourlyMeans <- function(system_gather) {
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather %>%
    group_by(streetlight, date, timeUse, id) %>%
    summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
}

system_hourly <- hourlyMeans(sl_new)
system_hourly <- spread(system_hourly, id, value)
system_hourly$timestamp <- as.POSIXct(paste(paste(system_hourly$date, 
                                                     system_hourly$timeUse), ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")
system_hourly <- system_hourly[,-c(2,3)] #Remove date and timeUse

system_hourly <- system_hourly[, c(12,1:11)]
colnames(system_hourly) <- c("timestamp","streetlight","Battery_power_W",
                        "Solar_charger_Battery_watts_W", "Charged_energy_W",
                        "Discharged_energy_W", "Light_load_W", "Load_current_A",  
                        "PV_DC_coupled_W", "PV_power_W", "Socket_load_W", "State_of_charge_%")
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(system_hourly, file="System_hourly_95_jul_mar.csv", row.names=FALSE)

###############################################################################
library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)
library(openxlsx)
library(wesanderson)

### Read in the system file
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
system_hourly <- read.csv("System_hourly_95_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
system_hourly$timestamp <- as.POSIXct(system_hourly$timestamp, tz="GMT", origin="1970-01-01")
system_hourly$date <- date(system_hourly$timestamp)
system_hourly$timeUse <- hour(system_hourly$timestamp)
system_hourly$month <- as.character(month(system_hourly$timestamp, label=TRUE, abbr=TRUE))

## Yield calculation 
### For each streetlight see what check for missing data - 1 July 2019 to 19 March 2020
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
all_hours <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), 
                                by = "1 hour"), "%H", tz="GMT")
all_hours <- all_hours[-25]

#Number of on hours per day for each streetlight id and each variable
sl_gather <- gather(system_hourly, "variable", "value", 3:12)
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

# Plot yield map for yield - yield calculated as % hours of total per day i.e. 24
sl_on_hours2$yield <- sl_on_hours2$onHours * 100.0 / 24.0
sl_on_hours2$id2 <- paste(as.character(sl_on_hours2$streetlight), 
                          as.character(sl_on_hours2$variable))

# Plotting a heat map for CPE
pal <- wes_palette("Zissou1", 100, type = "continuous")
sl_yield <- sl_on_hours2
sl_yield$variable <- as.character(sl_yield$variable)
sl_yield <- sl_yield[sl_yield$variable=="PV_power_W",]
sl_yield$id2 <- substr(sl_yield$id2, 1, 3)
sl_yield %>%
  ggplot(aes(date, id2)) + geom_tile(aes(fill = yield)) +
  scale_fill_gradientn(colours = pal) + 
  xlab("X axis") +
  ylab("Y axis") +
  labs(title="Yield for Nepal Streetlights: 01 Jul'19 - 19 Mar'20" , 
       y="Streetlight",
       x = "Day of study",
       fill="Yield") + 
  theme(axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size=6),
        plot.title = element_text(size=10))
ggsave("../Plots/Paper4/version3/yield_after_95.png")

######### Box plots for hourly data ###############
plotMeans <- function(df, lim1, lim2) {
  df %>%
    ggplot(aes(x=as.factor(timeUse), y=Socket_load_W)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)",
         x = "Hour of day") + 
    theme(plot.title = element_text(size=10), legend.position = "none") +
    scale_y_continuous(limits=c(lim1, lim2))
}

#### Box Plots for all SL #########
plotMeans(system_hourly[system_hourly$streetlight=="SL1",], 0, 30) + 
  labs(title="Mean hourly system and socket load at SL1 in Jul 2019 to Mar 2020")
ggsave("../Plots/Paper4/version3/sl1.png")

plotMeans(system_hourly[system_hourly$streetlight=="SL2",], 0, 20) + 
  labs(title="Mean hourly system and socket load at SL2 in Jul 2019 to Mar 2020")
ggsave("../Plots/Paper4/version3/sl2.png")

plotMeans(system_hourly[system_hourly$streetlight=="SL3",], 0, 15) + 
  labs(title="Mean hourly system and socket load at SL3 in Jul 2019 to Mar 2020")
ggsave("../Plots/Paper4/version3/sl3.png")

plotMeans(system_hourly[system_hourly$streetlight=="SL4",], 0, 15) + 
  labs(title="Mean hourly system and socket load at SL4 in Jul 2019 to Mar 2020")
ggsave("../Plots/Paper4/version3/sl4.png")

#plotMeans(system_hourly[system_hourly$streetlight=="SL5",], 0, 15) + 
#  labs(title="Mean hourly system and socket load at SL5 in Jul 2019 to Aug 2019")

plotMeans(system_hourly[system_hourly$streetlight=="SL6" &
                          (system_hourly$month!="Nov" & system_hourly$month!="Dec"),], 0, 20) + 
  labs(title="Mean hourly system and socket load at SL6 in Jul 2019 to Mar 2020 (excluding Nov and Dec 2019)")
ggsave("../Plots/Paper4/version3/sl6.png")

#plotMeans(system_hourly[system_hourly$streetlight=="SL7" &
#                          (system_hourly$month=="Jul" | system_hourly$month=="Aug" |
#                             system_hourly$month=="Sep"),], 0, 250) + 
#  labs(title="Mean hourly system and socket load at SL7 in Jul 2019 to Sep 2019")

#################################################################################################

#################################################################################################
#Get socket and system data on full data days
sl_sub <- system_hourly[,c(1,2,11,13,14)]
full_days <- sl_sub %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==24,]
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  df <- df[df$date %in% df_days,]
  sl_full <- rbind(sl_full, df)
}

#Get daily consumption
startDate <- as.Date("2019-06-30")
sl_all_daily <- sl_full %>%
  group_by(streetlight, date) %>%
  summarise(socketLoad = sum(Socket_load_W))
sl_all_daily <- as.data.frame(sl_all_daily)
sl_all_daily$days <- as.numeric(sl_all_daily$date - startDate)
sl_all_daily <- sl_all_daily[order(sl_all_daily$days),]

#Remove data for SL6 during Nov and Dec months
sl_all_daily <- sl_all_daily[!(sl_all_daily$streetlight=="SL6" & (sl_all_daily$date>"2019-10-31" & sl_all_daily$date<="2019-12-31")),]
#Remove data for SL5 and SL7 
sl_all_daily <- sl_all_daily[!(sl_all_daily$streetlight=="SL5" | sl_all_daily$streetlight=="SL7"),]

sl_all_daily %>%
    ggplot(aes(x=days, y=socketLoad, color=streetlight, shape=streetlight)) +
    geom_point() + 
    labs(title="Daily system and socket load at Nepal SL for full data days since commissioning: 1st Jul'19 to 19th Mar'20",
         y="Total energy consumption (Wh)",
         x = "Days since commissioning",
         colour="Streetlight",
         shape="Streetlight") + 
    scale_shape_manual(values=c(1,8,1,8,1,8,1)) +
    theme(plot.title = element_text(size=9),legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0,260,by=20)) +
  scale_y_continuous(breaks=seq(0,300,by=30))
ggsave("../Plots/Paper4/version3/dailyLoad.png")

#Getting daytime consumption on full days
sl_all_daytime <- system_hourly[,c(1,2,11,13,14)]
sl_all_daytime <- sl_all_daytime[sl_all_daytime$timeUse>=7 & sl_all_daytime$timeUse<=16,]
full_days <- sl_all_daytime %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==10,]
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  df <- df[df$date %in% df_days,]
  sl_full <- rbind(sl_full, df)
}

startDate <- as.Date("2019-06-30")
sl_daytime <- sl_full %>%
  group_by(streetlight, date) %>%
  summarise(socketLoad = sum(Socket_load_W))
sl_daytime <- as.data.frame(sl_daytime)
sl_daytime$days <- as.numeric(sl_daytime$date - startDate)
sl_daytime <- sl_daytime[order(sl_daytime$days),]

#Remove data for SL6 during Nov and Dec months
sl_daytime <- sl_daytime[!(sl_daytime$streetlight=="SL6" & (sl_daytime$date>"2019-10-31" & sl_daytime$date<="2019-12-31")),]
#Remove data for SL5 and SL7 
sl_daytime <- sl_daytime[!(sl_daytime$streetlight=="SL5" | sl_daytime$streetlight=="SL7"),]

sl_daytime %>%
  ggplot(aes(x=days, y=socketLoad, color=streetlight, shape=streetlight)) +
  geom_point() + 
  labs(title="Daily daytime (7:00 to 16:59) system and socket load at Nepal SL since commissioning: 1st Jul'19 to 19th Mar'20",
       y="Total energy consumption (Wh)",
       x = "Days since commissioning",
       colour="Streetlight",
       shape="Streetlight") + 
  scale_shape_manual(values=c(1,8,1,8,1,8,1)) +
  theme(plot.title = element_text(size=9),legend.position = "bottom")  +
  scale_x_continuous(breaks = seq(0,260,by=20)) +
  scale_y_continuous(breaks=seq(0,300,by=30))
ggsave("../Plots/Paper4/version3/daytimeLoad.png")

#################################################################################################
#Get full data days to get daily avg values for capture loss, system and socket load, light load, 
#system loss (PV power - total load)

#Read in weather data - corrected and stitched in nepal_sl_stitching_3Apr file.
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
weather_data <- read.csv("weather_hourly_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
weather_data$timestamp <- as.POSIXct(weather_data$timestamp, tz="GMT", origin="1970-01-01")
weather_data <- gather(weather_data, "streetlight", "Potential_PV_power_W", 2:8)
weather_data$date <- date(weather_data$timestamp)
weather_data$month <- as.character(month(weather_data$date, label=TRUE, abbr=TRUE))
weather_data$timeUse <- format(weather_data$timestamp, format='%H')

#Subset system data to include Light load, PV power and System socket load 
sl_sub <- system_hourly[,c(1,2,7,10,11,13,14,15)]
#Get complete cases
sl_sub <- sl_sub[complete.cases(sl_sub),]
#Get full days for all days with complete cases i.e. all values
full_days <- sl_sub %>%
  group_by(streetlight, date) %>%
  summarise(hours = length(timeUse))
full_days <- as.data.frame(full_days)
full_days <- full_days[full_days$hours==24,]

#Subset data to get only full days - we get 90% of the data when full data days are considered
sl_full <- data.frame()
for(i in 1:length(unique(full_days$streetlight))) {
  df <- sl_sub[sl_sub$streetlight == unique(full_days$streetlight)[i], ]
  weather <- weather_data[weather_data$streetlight == unique(full_days$streetlight)[i], ]
  df_days <- full_days$date[full_days$streetlight == unique(full_days$streetlight)[i]]
  df <- df[df$date %in% df_days,]
  weather_sub <- weather[weather$date %in% df_days,]
  df$Potential_PV_power_W <- weather_sub$Potential_PV_power_W
  sl_full <- rbind(sl_full, df)
}

#Calculate Capture loss
sl_full$Capture_loss_W <- sl_full$Potential_PV_power_W - sl_full$PV_power_W

#Calculate Total load
sl_full$Total_load_W <- abs(sl_full$Light_load_W) + sl_full$Socket_load_W

#Calculate System loss
sl_full$System_loss_W <- sl_full$PV_power_W - sl_full$Total_load_W

# Plotting box plots for Capture loss for SL1, SL3 and SL4
ggplot(sl_full[sl_full$streetlight=="SL1",], aes(as.factor(timeUse),Capture_loss_W)) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Capture loss at Nepal SL1 from July 2019 to 18th Mar 2020",y="Capture loss (W)", x = "Hour of day") + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(-250, 310), breaks=seq(-250,310,by=50)) 
ggsave("../Plots/Paper4/Capture_loss_SL1.png")

ggplot(sl_full[sl_full$streetlight=="SL2",], aes(as.factor(timeUse),Capture_loss_W)) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Capture loss at Nepal SL2 from July 2019 to 18th Mar 2020",y="Capture loss (W)", x = "Hour of day") + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(-250, 310), breaks=seq(-250,310,by=50)) 
ggsave("../Plots/Paper4/Capture_loss_SL2.png")

ggplot(sl_full[sl_full$streetlight=="SL3",], aes(as.factor(timeUse),Capture_loss_W)) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Capture loss at Nepal SL3 from July 2019 to 18th Mar 2020",y="Capture loss (W)", x = "Hour of day") + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(-250, 300), breaks=seq(-250,300,by=50)) 
ggsave("../Plots/Paper4/Capture_loss_SL3.png")

ggplot(sl_full[sl_full$streetlight=="SL4",], aes(as.factor(timeUse),Capture_loss_W)) +
  geom_boxplot(aes(fill=timeUse))  +
  labs(title="Capture loss at Nepal SL4 from July 2019 to 18th Mar 2020",y="Capture loss (W)", x = "Hour of day") + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits=c(-150, 310), breaks=seq(-150,310,by=50)) 
ggsave("../Plots/Paper4/Capture_loss_SL4.png")

#Calculate daily sum
sl_full <- gather(sl_full, "id", "value", c(3:5,9:12))
sl_full_daily <- sl_full %>%
  group_by(streetlight, month, date, id) %>%
  summarise(value = sum(value))
sl_full_daily <- as.data.frame(sl_full_daily)

#Typical day per month
sl_full_typical <- sl_full_daily %>%
  group_by(streetlight, month, id) %>%
  summarise(value = mean(value))
sl_full_typical <- as.data.frame(sl_full_typical)

#Remove SL6 data from Nov and Dec
sl_full_typical <- sl_full_typical[!(sl_full_typical$streetlight=="SL6" & 
                                       (sl_full_typical$month=="Nov" | sl_full_typical$month=="Dec")),]
sl_full_typical <- sl_full_typical[!(sl_full_typical$streetlight=="SL5" & 
                                       sl_full_typical$streetlight=="SL7"),]

sl <- data.frame()
for(i in 1:length(unique(sl_full_typical$month))) {
  df <- sl_full_typical[sl_full_typical$month == unique(sl_full_typical$month)[i],]
  if(df$month[1]=="Jul") {
    df$x_label <- paste("2019-07", df$streetlight, sep=" ")
  } else if(df$month[1]=="Aug") {
    df$x_label <- paste("2019-08", df$streetlight, sep=" ")
  } else if(df$month[1]=="Sep") {
    df$x_label <- paste("2019-09", df$streetlight, sep=" ")
  } else if(df$month[1]=="Oct") {
    df$x_label <- paste("2019-10", df$streetlight, sep=" ")
  } else if(df$month[1]=="Nov") {
    df$x_label <- paste("2019-11", df$streetlight, sep=" ")
  } else if(df$month[1]=="Dec") {
    df$x_label <- paste("2019-12", df$streetlight, sep=" ")
  } else if(df$month[1]=="Jan") {
    df$x_label <- paste("2020-01", df$streetlight, sep=" ")
  } else if(df$month[1]=="Feb") {
    df$x_label <- paste("2020-02", df$streetlight, sep=" ")
  } else if(df$month[1]=="Mar") {
    df$x_label <- paste("2020-03", df$streetlight, sep=" ")
  } 
  sl <- rbind(sl, df)
}
sl <- sl[sl$id=="Capture_loss_W" | sl$id=="Light_load_W" 
                      | sl$id=="Socket_load_W" | sl$id=="System_loss_W", ]
sl$value[sl$id=="Light_load_W"] <- abs(sl$value[sl$id=="Light_load_W"])
sl <- sl[order(sl$x_label),]
sl$label <- substr(sl$x_label, 1, 7)

plotSL <- function(df) {
  df %>%
    ggplot(aes(x = label, y= value/1000.0, fill = id)) +
    geom_bar(stat="identity", width=.3, position = "stack") + 
    labs(y="Consumed and potential electrical energy (kWh)",
         x = "Month",
         fill="Parameter")  +
    theme(plot.title = element_text(size=10), legend.position = "bottom", 
          legend.box = "horizontal",  legend.key.size = unit(0.5, "cm"), 
          legend.margin = margin(t=0,r=0,b=0,l=0), axis.text.x = element_text(angle = 0),
          axis.title = element_text(size=9))
}

plotSL(sl[sl$streetlight=="SL1",]) + 
  labs(title="Daily average electrical energy values at Nepal SL1: 1st Jul'19 to 19th Mar'20")
ggsave("../Plots/Paper4/version3/sl1_barplot.png")

plotSL(sl[sl$streetlight=="SL2",]) + 
  labs(title="Daily average electrical energy values at Nepal SL2: 1st Jul'19 to 19th Mar'20")
ggsave("../Plots/Paper4/version3/sl2_barplot.png")

plotSL(sl[sl$streetlight=="SL3",]) + 
  labs(title="Daily average electrical energy values at Nepal SL3: 1st Jul'19 to 19th Mar'20")
ggsave("../Plots/Paper4/version3/sl3_barplot.png")

plotSL(sl[sl$streetlight=="SL4",]) + 
  labs(title="Daily average electrical energy values at Nepal SL4: 1st Jul'19 to 19th Mar'20")
ggsave("../Plots/Paper4/version3/sl4_barplot.png")

plotSL(sl[sl$streetlight=="SL6",]) + 
  labs(title="Daily average electrical energy values at Nepal SL6: 1st Jul'19 to 19th Mar'20")
ggsave("../Plots/Paper4/version3/sl6_barplot.png")

#################################################################################################
############## Putting together data for the spreadsheet ########################################
#Irradiance, Potential PV power, Actual PV power, System and socket load, Light load, total load,
#Final yield (Yf), Reference yield (Yr), Actual Array yield (YA), System losses (Ls),
#Capture losses (Lc), Performance ratio, Production factor, Overall system efficiency

#For each SL get the number of full days per month from sl_full_daily
sl_days <- sl_full_daily[,-c(4:5)] %>%
  group_by(streetlight, month) %>%
  summarise(full_days = length(unique(date)))
sl_days <- sl_days[!(sl_days$streetlight=="SL6" & (sl_days$month=="Nov" | sl_days$month=="Dec")),]

###Using sl_full_typical
sl <- sl_full_typical
sl$value <- sl$value / 1000.0 #Converitng to kWh
sl <- spread(sl, id, value)
sl$Irradiance_kWhm2 <- sl$Potential_PV_power_W / (1.968 * 0.152)
sl$System_efficiency <- sl$Total_load_W / (sl$Potential_PV_power_W / 0.152)
sl$Full_days <- sl_days$full_days
sl <- sl[,c(1,2,12,10,5,6,7,4,9,8,3,11)]
colnames(sl) <- c("Streetlight","Month", "Full_days","Irradiance_kWh/m2","Potential_PV_power_kWh",
                  "Actual_PV_power_kWh","System_socket_load_kWh","Light_load_kWh",
                  "Total_load_kWh","System_losses_kWh","Capture_losses_kWh",
                  "System_efficiency")
write.csv(sl,"~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/Data_summary_Nepal.csv",
          row.names=FALSE)
