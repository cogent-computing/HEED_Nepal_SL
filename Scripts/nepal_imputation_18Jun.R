#******************************************************************************************#
# This is the script for imputing missing data for all Nepal SL                            #
# Author: K Bhargava                                                                       #
# Last updated on: 22nd Jun 2020                                                           #
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
#******************************************************************************************#

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("interpolation", "kalman")
# Impute missing values for SoC, Charged energy, Discharged energy, Battery watts, Load current, PV power, Battery power
variables <- c("Battery.Monitor.State.of.charge..","Charged.energy.W","Discharged.energy.W",
               "Solar.Charger.Battery.watts.W", "Solar.Charger.Load.current.A", 
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
      df1 <- as.data.frame(na_seadec(df.ts, algorithm=methodImpute[j],find_frequency=TRUE))
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
  mutate(Potential.PV.power.W = system_sub$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_imputed_data.csv"), row.names=FALSE)
#******************************************************************************************#

# Implement the chosen imputation technique

# Calculate socket and system load - remove from sl_analysis then
# System and socket load : sl_all$sysSocketLoad <- sl_all$Battery_watts_W - sl_all$Battery_power_W 
# Estimate system load and subtract to get socket load

# Calculate light load - remove from sl_analysis then

# Light load: sl_all$Light_load_W <- ifelse(sl_all$Battery_watts_W<0, sl_all$Battery_watts_W, 0)



# Apply corrections
# Corrections applied previously were: removing -ve load values; considering only 95% values