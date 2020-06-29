#******************************************************************************************#
# This is the script for imputing missing data for all Nepal SL                            #
# Author: K Bhargava                                                                       #
# Last updated on: 27th Jun 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(xts) # for converting data into time series
library(timeDate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read hourly data and subset data to choose only columns we need to impute
system_hourly <- read.csv(here(filepath,"raw_hourly_sl_data.csv"), header = TRUE, stringsAsFactors=FALSE)
system_hourly <- system_hourly %>% mutate(date = as.Date(date))
system_hourly <- system_hourly[!(system_hourly$streetlight=="SL5" & system_hourly$date>="2019-09-01"),]
#******************************************************************************************#

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
#methodImpute <- c("interpolation", "kalman")
methodImpute <- c("interpolation")
# Impute missing values for SoC, Charged energy, Discharged energy, Battery watts, Load current, PV power, Battery power
variables <- c("Battery.Monitor.State.of.charge..", "Solar.Charger.Battery.watts.W", 
               "Solar.Charger.PV.power.W","System.overview.Battery.Power.W", "System.overview.PV...DC.coupled.W")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  x <- system_hourly[c("streetlight","date","timeUse",variables[k])]
  for(i in seq_along(unique(x$streetlight))) {
    df <- x[x$streetlight == unique(x$streetlight)[i], ]
    
    # Convert data frame into a time series using xts to serve as input to na_seadec function
    # For this data, seasonality is 1 day with a reading every hour
    df.ts <- df[,-1]
    df.ts <- spread(df.ts, timeUse, variables[k])
    df.ts <- xts(df.ts[,-1], order.by=as.Date(df.ts[,1], "%Y-%m-%d"))
    
    # Impute data using different functions of na_seadec and bind to df
    for(j in seq_along(methodImpute)) {
      df1 <- as.data.frame(na_seadec(df.ts, algorithm=methodImpute[j], find_frequency=TRUE))
      df1 <- df1 %>% mutate(date=row.names(df1))
      df1 <- gather(df1, "timeUse", "value", 1:24)
      df1[is.na(df1)] <- 0
      df1 <- df1[order(df1$date),]
      df <- cbind(df, df1$value)
    }
    colnames(df) <- c(colnames(df)[1:3],paste(variables[k],"original",sep="_"),
                      paste(variables[k],methodImpute,sep="_"))
    df <- gather(df, "variable","value",4:6)
    
    # Bind data for all SL
    na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
  }
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = system_hourly$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_interpolated_data.csv"), row.names=FALSE)
#write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_kalman_data.csv"), row.names=FALSE)
#write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_kalman_data2.csv"), row.names=FALSE)
#write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_imputed_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Read in data files - interpolated data
na_seadec_interpolation <- read.csv(here(filepath,"na_seadec_interpolated_data.csv"), 
                                    header=TRUE, stringsAsFactors = FALSE)
na_seadec_interpolation <- na_seadec_interpolation %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp,tz="GMT",origin="1970-01-01"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Compute statistics for original and imputed data
na_seadec_sub <- gather(na_seadec_interpolation, "id", "value", c(4:14))
stats_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
stats_na_seadec_sub <- gather(stats_na_seadec_sub, "variable", "value", 3:7)
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$streetlight=="SL1" & 
                             stats_na_seadec_sub$variable=="sd",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))
write.csv(stats_na_seadec_sub, file=here(filepath,"stats_na_seadec.csv"), row.names=FALSE)

# Plot data for all variables
ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(2,1)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + 
  theme(legend.position = "bottom") + labs(x="Timestamp", y="State of charge (%)")
ggsave(here(plot_dir,"imputed_soc.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(4,3)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Battery power (Wh)")
ggsave(here(plot_dir,"imputed_battery_watts.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(6,5,11)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Power (Wh)")
ggsave(here(plot_dir,"imputed_pv_power.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(8,7)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Power (Wh)")
ggsave(here(plot_dir,"imputed_battery_power.png"))

ggplot(na_seadec_sub[na_seadec_sub$id==unique(na_seadec_sub$id)[c(10,9)],], 
       aes(timestamp, value, color=id)) + facet_wrap(~streetlight) + geom_line() + theme(legend.position = "bottom") + 
  labs(x="Timestamp", y="Power (Wh)")
ggsave(here(plot_dir,"imputed_pv_dc_power.png"))
#******************************************************************************************#

#******************************************************************************************#
# To use interpolation values and calculate +ve/-ve solar battery power imputed & original,
# +ve/-ve system battery power imputed and original 
# system and socket load (battery watts - battery power) imputed and original,
# light load (-ve battery power) imputed and original, and cap SoC and PV power >0
na_seadec_imputedData <- na_seadec_interpolation %>% 
  mutate(Positive.Solar.Battery.Power_interpolation=ifelse(Solar.Charger.Battery.watts.W_interpolation<0,0,
                                    Solar.Charger.Battery.watts.W_interpolation),
         Negative.Solar.Battery.Power_interpolation=ifelse(Solar.Charger.Battery.watts.W_interpolation>0,0,
                                    Solar.Charger.Battery.watts.W_interpolation),
         Positive.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original<0, 0,
                                    Solar.Charger.Battery.watts.W_original),
         Negative.Solar.Battery.Power_original=ifelse(Solar.Charger.Battery.watts.W_original>0, 0,
                                    Solar.Charger.Battery.watts.W_original),
         Charged.energy.W_interpolation=ifelse(System.overview.Battery.Power.W_interpolation<0, 0,
                                               System.overview.Battery.Power.W_interpolation),
         Discharged.energy.W_interpolation=ifelse(System.overview.Battery.Power.W_interpolation>0, 0,
                                                  System.overview.Battery.Power.W_interpolation),
         Charged.energy.W_original=ifelse(System.overview.Battery.Power.W_original<0,0,
                                          System.overview.Battery.Power.W_original),
         Discharged.energy.W_original=ifelse(System.overview.Battery.Power.W_original>0, 0,
                                             System.overview.Battery.Power.W_original),
         System.socket.load.W_interpolation=Solar.Charger.Battery.watts.W_interpolation-
           System.overview.Battery.Power.W_interpolation,
         System.socket.load.W_original=Solar.Charger.Battery.watts.W_original-
           System.overview.Battery.Power.W_original,
         Light.load.W_interpolation=ifelse(Solar.Charger.Battery.watts.W_interpolation<0,
                                           Solar.Charger.Battery.watts.W_interpolation, 0),
         Light.load.W_original=ifelse(Solar.Charger.Battery.watts.W_original<0,
                                      Solar.Charger.Battery.watts.W_original, 0),
         Battery.Monitor.State.of.charge.._interpolation=ifelse(Battery.Monitor.State.of.charge.._interpolation<100,
                              Battery.Monitor.State.of.charge.._interpolation, 100),
         Solar.Charger.PV.power.W_interpolation=ifelse(Solar.Charger.PV.power.W_interpolation<0,0,
                         Solar.Charger.PV.power.W_interpolation))
 
# Plot box plots for socket and system load to estimate the system load
# Subtract hourly values as they are needed for typical day plots
systemLoad_day <- list(c(7.6,7.6,7,7,8,7,7,7.2,7.4),
                       c(7,6.5,6.5,7,7,6.6,6.7,6.8,7.2),
                       c(2.8,1.2,1.5,3.3,3.2,2.7,2.7,0.4,4.2),
                       c(1.6,1.5,1.3,0.3,2,2.7,0.5,0.5,0.5),
                       c(3.5,7.4),
                       c(3.6,3.1,3.2,3.3,3.2,3.3,3.4,3.5,3.7),
                       c(0,0,10,0,0,0,0,0,0)) # For SL1 to SL7 between 7 to 18 hours 
systemLoad_eve <- list(c(9,9,8.75,7.8,8.2,8.3,8.3,8.5,8.2),
                       c(8.5,8.5,8.2,8.2,7.8,7.8,7.75,7.9,8),
                       c(4.5,4.5,4.2,4.2,4.2,4,4,1.5,2.8),
                       c(3,2.7,3.7,2.7,4.3,4.4,2.5,2.5,2.5),
                       c(4,7.25),
                       c(4.7,5,4.9,4.8,4.8,5.5,5,5.2,5),
                       c(0,0,10,0,0,0,0,0,0)) # For SL1 to SL7 between 19 to 6 hours
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Jul",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(-50,200,10), limits=c(-50,200))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Aug",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(-50,200,10), limits=c(-50,200))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Sep",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(-50,200,10), limits=c(-50,200))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Oct",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Nov",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()   +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Dec",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Jan",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Feb",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()   +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Mar",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1))

na_seadec_correctedData <- data.frame()
for(i in seq_along(unique(na_seadec_imputedData$streetlight))) {
  x <- na_seadec_imputedData[na_seadec_imputedData$streetlight==
                                unique(na_seadec_imputedData$streetlight)[i],]
  
  # Calculate socket load by removing the estimated system load 
  # For SL7 in July and Aug, socket load is positive system and socket load
  df <- data.frame()
  for(j in seq_along(unique(x$month))) {
    df_sub <- x[x$month==unique(x$month)[j],]
    df_sub <- df_sub %>% 
      mutate(Socket.load.W_interpolation=ifelse((i==7 & (j==1 | j==2)), 
             ifelse(System.socket.load.W_interpolation<0, 0,
                    System.socket.load.W_interpolation),
             ifelse(timeUse>=7 & timeUse<=18,
                    System.socket.load.W_interpolation-systemLoad_day[[i]][j],
                    System.socket.load.W_interpolation-systemLoad_eve[[i]][j])),
             Socket.load.W_original=ifelse((i==7 & (j==1 | j==2)),
                ifelse(System.socket.load.W_original<0, 0,
                       System.socket.load.W_original),
               ifelse(timeUse>=7 & timeUse<=18,
                  System.socket.load.W_original-systemLoad_day[[i]][j],
                  System.socket.load.W_original-systemLoad_eve[[i]][j])))
    df <- rbind(df, df_sub)
  }
  
  # Calculate SoC of battery and apply upper and lower thresholds - 
  # discharge of up to 20% acceptable
  state <- df$Battery.Monitor.State.of.charge.._interpolation[1] * 1954 / 100
  for(j in 2:length(df$date)) {
    soc <- state[j-1] + df$System.overview.Battery.Power.W_interpolation[j]
    if(soc>1954) {
      soc <- 1954
    } else if(soc<391) {
      soc <- 391
    }
    state <- append(state, soc)
  }
  df$State.of.Charge.W_interpolation <- state # in Wh
  
  # Use SoC to correct Actual PV power, Solar Battery power, System Battery power, 
  # Actual Socket load, +ve/-ve actual solar battery power, +ve/-ve actual battery power
  # Actual light load
  df <- df %>% 
    mutate(Actual.PV.power.W_interpolation=ifelse((State.of.Charge.W_interpolation==391 & 
                    is.na(Solar.Charger.PV.power.W_original)), 0, 
                    Solar.Charger.PV.power.W_interpolation),
           Actual.Solar.Charger.Battery.Power.W_interpolation=
             ifelse((State.of.Charge.W_interpolation==391 & 
                       is.na(Solar.Charger.Battery.watts.W_original)), 0, 
                    Solar.Charger.Battery.watts.W_interpolation),
           Actual.Battery.power.W_interpolation=ifelse((State.of.Charge.W_interpolation==391& 
                      is.na(System.overview.Battery.Power.W_original)), 0, 
                      System.overview.Battery.Power.W_interpolation),
           Actual.Socket.load.W_interpolation=ifelse((State.of.Charge.W_interpolation==391 & 
                  is.na(Socket.load.W_original)), 0, Socket.load.W_interpolation),
           Postive.Actual.Solar.Charger.Battery.Power.W_interpolation=
             ifelse(Actual.Solar.Charger.Battery.Power.W_interpolation<0, 0,
                    Actual.Solar.Charger.Battery.Power.W_interpolation),
           Negative.Actual.Solar.Charger.Battery.Power.W_interpolation=
             ifelse(Actual.Solar.Charger.Battery.Power.W_interpolation>0, 0,
                    Actual.Solar.Charger.Battery.Power.W_interpolation),
           Positive.Actual.Battery.Power.W_interpolation=
             ifelse(Actual.Battery.power.W_interpolation<0, 0, 
                    Actual.Battery.power.W_interpolation),
           Negative.Actual.Battery.Power.W_interpolation=
             ifelse(Actual.Battery.power.W_interpolation>0, 0, 
                    Actual.Battery.power.W_interpolation),
           Actual.Light.load.W_interpolation=
             Negative.Actual.Solar.Charger.Battery.Power.W_interpolation)
  
  # Bind data for all SL
  na_seadec_correctedData <- rbind(na_seadec_correctedData, df)
}
# Save corrected data
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Read data and calculate +ve/-ve battery power values
na_seadec_correctedData <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors=FALSE)
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp, tz="GMT", origin="1970-01-01"),
         month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                       labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Get full data days from original data - all variables needed
system_sub_original <- na_seadec_correctedData[,c(1:3,5,7,9,11,17)]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(streetlight, month2, date) %>% 
  summarise(hours=length(Solar.Charger.PV.power.W_original))
onHours <- as.data.frame(onHours[onHours$hours==24,])
full_days <- onHours %>% group_by(streetlight, month2) %>% summarise(days=length(date)) 
write.csv(full_days, file=here(filepath,"full_days_all_data_nepal.csv"), row.names=FALSE)

# Get typical day values for each SL
# Subset data to get SL, Time, Potential PV, SoC, Actual PV power, Actual Socket load, 
# Positive actual Solar battery power (E_a), Positive and negative actual battery power,
# Actual light load
system_sub_interpolation <- na_seadec_correctedData[,c(1,3,14,4,32,33,36,37,39,40,41)]
system_sub_original <- na_seadec_correctedData[,c(1,3,14,5,32,33,36,37,39,40,41)]

system_sub_original <- system_sub_original %>% 
  mutate(load = abs(Actual.Socket.load.W_interpolation) + 
           abs(Actual.Light.load.W_interpolation),
         loss = Potential.PV.power.W - 
           Postive.Actual.Solar.Charger.Battery.Power.W_interpolation,
         State.of.Charge.W_interpolation=State.of.Charge.W_interpolation*100/1954)
system_sub_original <- system_sub_original[,-c(7,11)] # Remove socket and light load
colnames(system_sub_original) <- c("streetlight","timeUse","E_p", "BM_SoC","SoC",
                                        "PV","E_a","B_cp","B_dp","E_load","L_c")

# Calculate Total load (actual AC + actual light), capture loss (potential-E_a)
system_sub_interpolation <- system_sub_interpolation %>% 
  mutate(load = abs(Actual.Socket.load.W_interpolation) + 
           abs(Actual.Light.load.W_interpolation),
         loss = Potential.PV.power.W - 
           Postive.Actual.Solar.Charger.Battery.Power.W_interpolation,
         State.of.Charge.W_interpolation=State.of.Charge.W_interpolation*100/1954)
system_sub_interpolation <- system_sub_interpolation[,-c(7,11)] # Remove socket and light load
colnames(system_sub_interpolation) <- c("streetlight","timeUse","E_p", "BM_SoC","SoC",
                                        "PV","E_a","B_cp","B_dp","E_load","L_c")

# Calculate typical values for each SL
system_sub_original <- gather(system_sub_original, id, value, 3:11)
system_typical_original <- system_sub_original %>% 
  group_by(streetlight, timeUse, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_typical_original <- as.data.frame(system_typical_original)
system_typical_original <- spread(system_typical_original, id, value)

system_sub_interpolation <- gather(system_sub_interpolation, id, value, 3:11)
system_typical_interpolation <- system_sub_interpolation %>% 
  group_by(streetlight, timeUse, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_typical_interpolation <- as.data.frame(system_typical_interpolation)
system_typical_interpolation <- spread(system_typical_interpolation, id, value)

# Plot typical values for each SL
plotTypical <- function(df) {
  ggplot(df, aes(x=timeUse)) + geom_line(aes(y=B_cp/1000.0, color="B_cp", linetype="B_cp")) +
    geom_line(aes(y=abs(B_dp)/1000.0, color="B_dp",linetype="B_dp")) + 
    geom_line(aes(y=E_a/1000.0, color="E_a",linetype="E_a")) +
    geom_line(aes(y=E_load/1000.0, color="E_load",linetype="E_load")) + 
    geom_line(aes(y=E_p/1000.0, color="E_p",linetype="E_p")) +
    geom_line(aes(y=L_c/1000.0, color="L_c",linetype="L_c")) + 
    scale_y_continuous(breaks= seq(0,0.25,0.05), sec.axis = sec_axis(~.*400, name = "State of Charge (%)")) +
    labs(y="Energy (kWh)", x = "Time of day", colour="Parameter", linetype="Parameter") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + theme(plot.title = element_text(size=10), legend.position = "bottom",
                                                      legend.box = "horizontal",  legend.key.size = unit(0.6, "cm"), 
                                                      legend.margin = margin(t=0,r=0,b=0,l=0),
                                                      axis.text = element_text(size=10), axis.title = element_text(size=12))
}
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL1",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL1 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl1_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL2",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL2 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl2_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL3",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL3 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl3_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL4",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL4 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl4_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL5",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL5 between July 2019 and Aug 2019")
ggsave(here(plot_dir,"typical_day_sl5_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL6",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL6 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl6_imputed_interpolation.png"))
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL7",]) +
  geom_line(aes(y = SoC/400, color = "SoC",linetype="SoC")) +
  labs(title="Actual typical day profile for Nepal SL7 between July 2019 and Mar 2020")
ggsave(here(plot_dir,"typical_day_sl7_imputed_interpolation.png"))
#******************************************************************************************#

#******************************************************************************************#
# Calculate daily data - Potential PV, PV power (original and imputed), Socket load (imputed), 
# +ve/-ve solar charger battery watts (original and imputed), 
# +ve/-ve system battery power (original and imputed), Light load

# Calculate daily data - PV power (original and imputed), Socket load (original and imputed), 
# Potential PV, actual PV power, actual socket load, 
# +ve/-ve solar charger battery watts (original and imputed), +ve/-ve system battery power 
# (original and imputed), +ve/-ve actual solar charger battery power, 
# +ve/-ve actual battery power, light demand and actual light load
na_seadec_sub <- na_seadec_correctedData[,c(1:3,17,8:9,30:31,14,33,36,18:21,22:25,37:40,28,41)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:25))
system_daily <- na_seadec_sub %>% group_by(streetlight, month2, date, id) %>% 
  summarise(value=sum(value, na.rm=TRUE))
system_daily <- as.data.frame(system_daily)

# Calculate daily value of SoC and BM_SoC -  take mean for the day
na_seadec_sub <- na_seadec_correctedData[,c(1:3,17,4:5,32)]
# Calculate daily loads
na_seadec_sub <- gather(na_seadec_sub, id, value, c(5:7))
system_daily_soc <- na_seadec_sub %>% group_by(streetlight, month2, date, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_daily_soc <- as.data.frame(system_daily_soc)

# Bind data sets
system_daily <- rbind(system_daily, system_daily_soc)

# Save data
system_daily <- spread(system_daily, id, value)
system_daily <- system_daily %>% mutate(State.of.Charge.W_interpolation = 
                                          State.of.Charge.W_interpolation*100/1954)
write.csv(system_daily, file=here(filepath,"system_daily_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#

#*****************************************************************************************#
# Monthly daily avg 
system_daily <- gather(system_daily, id, value, 4:27)
system_monthly <- system_daily %>% group_by(streetlight, month2, id) %>% 
  summarise(value=mean(value, na.rm=TRUE))
system_monthly <- as.data.frame(system_monthly)
# Converting power from W to Wh
system_monthly <- system_monthly %>% 
  mutate(value=ifelse(id=="Battery.Monitor.State.of.charge.._interpolation" |
                      id=="Battery.Monitor.State.of.charge.._original" |
                      id=="State.of.Charge.W_interpolation", value, value/1000.0))
# Consider absolute values for all variables
system_monthly <- system_monthly %>% mutate(value=abs(value))
system_monthly <- spread(system_monthly, id, value)
system_monthly <- system_monthly[order(system_monthly$streetlight, system_monthly$month2),]
write.csv(system_monthly, file=here(filepath,"monthly_avg_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#