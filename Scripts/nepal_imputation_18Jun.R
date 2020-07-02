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
    df <- gather(df, "variable","value",4:5)
    
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
systemLoad_day <- list(c(7.6,7.6,7,7,7.1,7.1,7,7.2,7.4),
                       c(7.1,6.7,6.5,7,7,6.6,6.7,6.8,7.2),
                       c(2.8,1.2,1.5,3.3,3.2,2.7,2.7,0.4,4.3),
                       c(1.6,1.5,1.3,0.3,2,2.2,0.5,0.5,0.5),
                       c(3.5,7.25),
                       c(3.6,3.0,3.2,3.3,3.2,3.3,3.4,3.5,3.7),
                       c(0,0,10,0,0,0,0,0,0)) # For SL1 to SL7 between 7 to 18 hours 
systemLoad_eve <- list(c(9,9,8.75,8,8.2,8.2,8.3,8.5,8.2),
                       c(8.5,8.5,8.2,8.1,7.8,7.8,7.75,7.9,8),
                       c(4.5,4.5,4.2,4.2,4.0,4.0,4.0,1.5,2.8),
                       c(3,2.75,3.7,2.7,4.3,4.4,2.5,2.5,2.5),
                       c(4,7.4),
                       c(4.7,5.0,5.0,4.8,5.0,5.5,5,5.2,5),
                       c(0,0,10,0,0,0,0,0,0)) # For SL1 to SL7 between 19 to 6 hours
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL7" &
                               na_seadec_imputedData$month=="Sep",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(-50,200,10), limits=c(-50,200))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Aug",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Sep",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot() +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Oct",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Nov",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()   +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Dec",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Jan",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Feb",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()   +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))
ggplot(na_seadec_imputedData[na_seadec_imputedData$streetlight=="SL1" &
                               na_seadec_imputedData$month=="Mar",], 
       aes(as.factor(timeUse), System.socket.load.W_interpolation)) + geom_boxplot()  +
  scale_y_continuous(breaks=seq(0,20,1), limits=c(0,20))

na_seadec_correctedData <- data.frame()
for(i in seq_along(unique(na_seadec_imputedData$streetlight))) {
  x <- na_seadec_imputedData[na_seadec_imputedData$streetlight==
                                unique(na_seadec_imputedData$streetlight)[i],]
  
  # Calculate socket load by removing the estimated system load - consider night time load=0
  # For SL7 in July and Aug, socket load is positive system and socket load, in sep remove
  # 10 Wh from daytime load and for all other months load is 0
  df <- data.frame()
  for(j in seq_along(unique(x$month))) {
    df_sub <- x[x$month==unique(x$month)[j],]
    if(unique(df_sub$streetlight)!="SL7") {
      df_sub <- df_sub %>% 
        mutate(Socket.load.W_interpolation=ifelse(timeUse>=7 & timeUse<=18,
                System.socket.load.W_interpolation-systemLoad_day[[i]][j], 0),
               Socket.load.W_original=ifelse(timeUse>=7 & timeUse<=18,
                System.socket.load.W_original-systemLoad_day[[i]][j], 0))
      df <- rbind(df, df_sub)
    } else {
      if(unique(df_sub$month)=="Jul" | unique(df_sub$month)=="Aug") {
        df_sub <- df_sub %>% 
          mutate(Socket.load.W_interpolation=ifelse(timeUse>=7 & timeUse<=18,
                  ifelse(System.socket.load.W_interpolation<0,0,
                         System.socket.load.W_interpolation),0),
                 Socket.load.W_original=ifelse(timeUse>=7 & timeUse<=18,
                   ifelse(System.socket.load.W_original<0,0,
                          System.socket.load.W_original),0))
      } else if(unique(df_sub$month)=="Sep") {
        df_sub <- df_sub %>% 
          mutate(Socket.load.W_interpolation=ifelse(timeUse>=7 & timeUse<=18,
                   ifelse(System.socket.load.W_interpolation<0,0,
                        System.socket.load.W_interpolation-10),0),
                 Socket.load.W_original=ifelse(timeUse>=7 & timeUse<=18,
                   ifelse(System.socket.load.W_original<0,0,
                          System.socket.load.W_original-10),0))
      } else {
        df_sub <- df_sub %>% mutate(Socket.load.W_interpolation=0,
                                    Socket.load.W_original=0)
      }
      df <- rbind(df, df_sub)
    }
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
# Remove -ve socket load values
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(Actual.Socket.load.W_interpolation=ifelse(Actual.Socket.load.W_interpolation<0,0,
                                                   Actual.Socket.load.W_interpolation))
# Save corrected data
write.csv(na_seadec_correctedData, file=here(filepath,"na_seadec_correctedData.csv"), row.names=FALSE)
#******************************************************************************************#