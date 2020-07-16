#******************************************************************************************#
# This is the script for pre-processing data from all Nepal SL to convert to hourly data   #
# analyse yield of hourly data and explore imputation techniques                           #
# Author: K Bhargava                                                                       #
# Last updated on: 6th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library("imputeTS") # for na_seadec imputation
library(mgcv) # for gam model-based imputation
library(xts)
library(MLmetrics) #for RMSE
library(timeDate) #for skewness
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots
THEME <- theme(legend.position = "bottom", legend.text=element_text(size=10, family="Times New Roman"),
               legend.key.size = unit(0.5, "cm"),legend.margin = margin(t=0,r=0,b=0,l=0), 
               panel.grid.major.y = element_line(colour="grey"), 
               panel.grid.minor = element_blank(), panel.background = element_blank(), 
               axis.line = element_line(colour = "black"), 
               axis.text = element_text(size=9, family="Times New Roman"),
               axis.title = element_text(size=10, family="Times New Roman")) 
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data"
plot_dir <- "Plots/Paper 7"
#******************************************************************************************#

#******************************************************************************************#
# Read SL and weather data between July 2019 to March 2020
sl_nepal <- read_csv(here(filepath,"sl_all_raw_jul19_mar20.csv"), col_names = TRUE)
sl_nepal <- as.data.frame(sl_nepal[,seq_along(sl_nepal)])
sl_nepal <- sl_nepal %>% mutate(date=date(timestamp),
                                month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), 
                            timeUse = hour(timestamp),
                            Charged.energy.W = ifelse(System.overview.Battery.Power.W<0, 0, 
                                                 System.overview.Battery.Power.W),
                            Discharged.energy.W = ifelse(System.overview.Battery.Power.W>0,0,
                                                         System.overview.Battery.Power.W)) 

weather_data <- read_csv(here(filepath,"weather_hourly_jul19_mar20.csv"), col_names = TRUE)
weather_data <- weather_data[,1:8]
weather_data <- gather(weather_data, "streetlight", "Potential_PV_power_W", 2:8)
weather_data$date <- date(weather_data$timestamp)
weather_data$timeUse <- hour(weather_data$timestamp)
weather_data <- weather_data[,-1] # Remove time stamp
weather_data <- weather_data[,c(1,3,4,2)] # Rearrange columns
#******************************************************************************************#

#******************************************************************************************#
# Plot yield per hour for each day - freq of data collection changes in Oct 2019
sl_all <- gather(sl_nepal, "id", "value", c(3:8,12:13))
sl_qual <- sl_all %>% group_by(streetlight, date, timeUse, id) %>% summarise(count = length(unique(timestamp)))
sl_qual <- as.data.frame(sl_qual)
sl_qual <- sl_qual %>% mutate(count2 = ifelse(count<100, count, 100),
                              yield=ifelse(date>="2019-10-15" & timeUse>=12, count*100/60, count*100/4),
                              yield2 =ifelse(yield>100, 100, yield))
pal <- wes_palette("Zissou1", 100, type = "continuous")
plotYield <- function(df) {
  ggplot(df, aes(date, timeUse)) + facet_wrap(~streetlight, nrow=2) + geom_tile(aes(fill = yield2)) + 
    scale_fill_gradientn(colours = pal, breaks=c(0,25,50,75,100)) + 
    scale_y_continuous(breaks=seq(0,24,by=4)) + xlab("X axis") + ylab("Y axis") + 
    labs(y="Time of day", x = "Day of study", fill="Yield (%)") + THEME + 
    guides(fill = guide_colorbar(barwidth = 8, barheight = 0.5))
}
# labs(title="Yield per hour for Nepal SL1-4: 1 Jul 2019 - 31 Mar 2020")
plotYield(sl_qual[sl_qual$streetlight%in%unique(sl_qual$streetlight)[1:4] & 
                    sl_qual$id=="System.overview.Battery.Power.W",]) 
ggsave(here(plot_dir,"yield_hourly1.pdf"), width = 8, height = 8, units = "cm")

# labs(title="Yield per hour for Nepal SL5-7: 1 Jul 2019 - 31 Mar 2020")
plotYield(sl_qual[sl_qual$streetlight%in%unique(sl_qual$streetlight)[5:7] & 
                    sl_qual$id=="System.overview.Battery.Power.W",]) 
ggsave(here(plot_dir,"yield_hourly2.pdf"), width = 8, height = 8, units = "cm")

# labs(title="Yield per hour for Nepal streetlights: 1 Jul 2019 - 31 Mar 2020")
plotYield(sl_qual[sl_qual$id=="System.overview.Battery.Power.W",]) 
ggsave(here(plot_dir,"yield_hourly_all.pdf"), width = 8, height = 8, units = "cm")
#******************************************************************************************#

#******************************************************************************************#
# Convert data into hourly mean, fill in NA for all days missing, add Potential PV data and save
system_hourly <- sl_all %>% group_by(streetlight, date, timeUse, id) %>% 
  summarise(value = mean(value, na.rm = TRUE))
system_hourly <- as.data.frame(system_hourly)
system_hourly <- spread(system_hourly, id, value)
system_hourly[is.na(system_hourly)] <- NA

# For each streetlight see what check for missing data - 1 July 2019 to 31 March 2020 = 275 days
all_days <- seq(as.Date("2019-07-01"), as.Date("2020-03-31"), by="days")
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
#write.csv(missingData, file=here(filepath,"missing_sl_data.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Get rows where NA values are a certain proportion of variables
system_hourly$na_count <- apply(system_hourly, 1, function(x) sum(is.na(x)))
# How many have 1 or more missing variables
sum(system_hourly$na_count>0) #9215*100/44184 = 20% rows
#******************************************************************************************#

#******************************************************************************************#
# Plot hourly data to see trend and seasonality - seasonality in data is 1 day as expected
# Add timestamp and month to the data
system <- system %>% mutate(month = as.character(month(date, label=TRUE, abbr=TRUE)),
                            timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                                      paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
                            month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                            labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Plot hourly values against time: SL1 in Oct 2019
subSystem <- system[system$streetlight=="SL1" &(system$id=="Solar.Charger.PV.power.W" | system$id=="Potential_PV_power_W"|
             system$id=="System.overview.Battery.Power.W" | system$id=="Solar.Charger.Battery.watts.W"),]
subSystem <- spread(subSystem, id, value)
colnames(subSystem) <- c(colnames(subSystem)[1:8], "PV.power.W", colnames(subSystem)[10])
subSystem <- gather(subSystem, "id", "value", 7:10)
ggplot(subSystem[subSystem$month=="Oct",], aes(timestamp, value)) + facet_wrap(~id) + geom_line() + 
  labs(x="Date", y="Energy (Wh)", title = "Energy profile of SL1 in Nepal in Oct 2019")

# Plot hourly values against time
plotHourly <- function(df) {
  ggplot(df[(df$id=="Solar.Charger.PV.power.W"|df$id=="Potential_PV_power_W"|df$id=="System.overview.Battery.Power.W"),], 
         aes(timestamp, value, color=as.factor(id))) + facet_wrap(~month2, scales = "free") + 
    geom_line(aes(linetype=factor(id))) + theme(legend.position = "bottom") + 
    labs(x="Day of study", y="Energy (Wh)", color="Variable", linetype="Variable")
}
plotHourly(system[system$streetlight=="SL1",]) + 
  labs(title = "Energy profile of SL1 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl1.png"))

plotHourly(system[system$streetlight=="SL2",]) + 
  labs(title = "Energy profile of SL2 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl2.png"))

plotHourly(system[system$streetlight=="SL3",]) + 
  labs(title = "Energy profile of SL3 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl3.png"))

plotHourly(system[system$streetlight=="SL4",]) + 
  labs(title = "Energy profile of SL4 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl4.png"))

plotHourly(system[system$streetlight=="SL5",]) + 
  labs(title = "Energy profile of SL5 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl5.png"))

plotHourly(system[system$streetlight=="SL6",]) + 
  labs(title = "Energy profile of SL6 in Nepal: 01 Jul'19 to 31 Mar'20")
ggsave(here(plot_dir,"energy_profile_sl6.png"))

plotHourly(system[system$streetlight=="SL7",]) + 
  labs(title = "Energy profile of SL7 in Nepal: 01 Jul'19 to 31 Mar'20")
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
#******************************************************************************************#
# Get full data days for all SL
df <- system_hourly[,c(1,2,3,4,5,6,7,8,9,10,13)]
df <- df[df$na_count==0,-11]
sl_on_hours <- df %>% group_by(streetlight, date) %>% summarise(onHours = length(System.overview.Battery.Power.W))
sl_on_hours <- as.data.frame(sl_on_hours[sl_on_hours$onHours==24,])
# Consider all full days
full_data <- data.frame()
for(i in seq_along(unique(sl_on_hours$streetlight))) {
  x <- df[df$streetlight == unique(sl_on_hours$streetlight)[i],]
  days <- sl_on_hours$date[sl_on_hours$streetlight == unique(sl_on_hours$streetlight)[i]]
  x <- x[x$date %in% days,]
  full_data <- rbind(full_data, x)
}
full_data <- full_data[1:13416,]

# Create a copy of full data and replace 20% data with NA 
incomplete_data <- data.frame()
for(i in seq_along(unique(full_data$streetlight))) {
  df_sub <- full_data[full_data$streetlight == unique(full_data$streetlight)[i],]
  n <- length(df_sub$streetlight)
  ind <- sample( c(1:n), floor(n/5))
  df_sub[ind,c("Battery.Monitor.State.of.charge..","Charged.energy.W","Discharged.energy.W",
               "Solar.Charger.Battery.watts.W", "Solar.Charger.Load.current.A", 
               "Solar.Charger.PV.power.W","System.overview.Battery.Power.W")] <- NA
  incomplete_data <- rbind(incomplete_data, df_sub)
}
#******************************************************************************************#

# Try different imputation techniques and compare performance using RMSE and MAPE metrics

#******************************************************************************************#
# Imputation using na_seadec owing to seasonality - works on univariate time series
methodImpute <- c("random", "mean", "locf", "interpolation", "ma", "kalman")
# Impute missing values for SoC, Charged energy, Discharged energy, Battery watts, Load current, PV power, Battery power
variables <- c("Battery.Monitor.State.of.charge..","Charged.energy.W","Discharged.energy.W",
               "Solar.Charger.Battery.watts.W", "Solar.Charger.Load.current.A", 
               "Solar.Charger.PV.power.W","System.overview.Battery.Power.W")
na_seadec_imputedData <- data.frame()
for(k in seq_along(variables)) {
  x <- incomplete_data[c("streetlight","date","timeUse",variables[k])]
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
    df <- gather(df, "variable","value",4:10)
    
    # Bind data for all SL
    na_seadec_imputedData <- rbind(na_seadec_imputedData, df)
  }
}
na_seadec_imputedData <- spread(na_seadec_imputedData, variable, value)
na_seadec_imputedData <- na_seadec_imputedData %>% 
  mutate(Potential.PV.power.W = incomplete_data$Potential_PV_power_W,
         month = as.character(lubridate::month(date, label=TRUE, abbr=TRUE)),
         timestamp = as.POSIXct(paste(date, ifelse(timeUse<10, paste("0",timeUse,":00:00",sep=""), 
                                                   paste(timeUse,":00:00",sep="")), sep=" "), origin="1970-01-01",tz="GMT"),
         month2 = factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                         labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
write.csv(na_seadec_imputedData, file=here(filepath,"na_seadec_test_imputed_data.csv"), row.names=FALSE)

# Calculating RMSE to get performance of na_seadec approach for each SL
na_seadec_imputedData <- na_seadec_imputedData %>%
  mutate(Battery.Monitor.State.of.charge..=full_data$Battery.Monitor.State.of.charge..,
         Charged.energy.W=full_data$Charged.energy.W, Discharged.energy.W=full_data$Discharged.energy.W,
         Solar.Charger.Battery.watts.W=full_data$Solar.Charger.Battery.watts.W,
         Solar.Charger.Load.current.A=full_data$Solar.Charger.Load.current.A,
         Solar.Charger.PV.power.W=full_data$Solar.Charger.PV.power.W, 
         System.overview.Battery.Power.W=full_data$System.overview.Battery.Power.W)
# Subset data where original values are na
na_seadec_sub <- na_seadec_imputedData[is.na(na_seadec_imputedData$Battery.Monitor.State.of.charge.._original),]
# Performance considering the sub set
perf_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight) %>%
  summarise_at(vars(matches("Battery.Monitor.State.of.charge.._")), ~RMSE(.x, Battery.Monitor.State.of.charge..)) %>% 
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("Solar.Charger.Battery.watts.W_")), ~RMSE(.x, Solar.Charger.Battery.watts.W))) %>%
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("Solar.Charger.Load.current.A_")), ~RMSE(.x, Solar.Charger.Load.current.A))) %>% 
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("Solar.Charger.PV.power.W_")), ~RMSE(.x, Solar.Charger.PV.power.W))) %>%
  bind_cols(na_seadec_sub %>% group_by(streetlight) %>% 
              summarise_at(vars(matches("System.overview.Battery.Power.W_")), ~RMSE(.x, System.overview.Battery.Power.W)))
# Remove streetlight columns that are redundant and original data
perf_na_seadec_sub <- perf_na_seadec_sub[,-c(7,9,15,17,23,25,31,33,39)]
perf_na_seadec_sub <- gather(perf_na_seadec_sub, id, value, 2:31)
ggplot(perf_na_seadec_sub[perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(1:6)],], aes(id, value)) + 
  facet_wrap(~streetlight) + geom_bar(stat="identity", width=.3, position = "dodge") + 
  theme(axis.text.x = element_text(angle=45)) + scale_x_discrete(labels=c("Interpolation","Kalman","LOCF","MA","Mean",
                            "Random")) + labs(y="SoC (%)",x="na_seadec approach", title="RMSE for SoC using na_seadec")

ggplot(perf_na_seadec_sub[perf_na_seadec_sub$streetlight=="SL3" & perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(7:12,19:30)],], aes(id, value)) + 
  geom_bar(stat="identity", width=.3, position = "dodge") + 
  scale_x_discrete(labels=c("Solar_battery_power_interpolation","Solar_battery_power_kalman","Solar_battery_power_LOCF",
                            "Solar_battery_power_ma","Solar_battery_power_mean","Solar_battery_power_random",
                            "PV_power_interpolation","PV_power_kalman","PV_power_LOCF","PV_power_ma",
                            "PV_power_mean","PV_power_random","System_battery_power_interpolation",
                            "System_battery_power_kalman","System_battery_power_LOCF","System_battery_power_ma",
                            "System_battery_power_mean","System_battery_power_random")) +
  theme(axis.text.x = element_text(angle=45)) + labs(y="RMSE (W)",x="na_seadec approach", title="RMSE using na_seadec")

ggplot(perf_na_seadec_sub[perf_na_seadec_sub$id %in% unique(perf_na_seadec_sub$id)[c(13:18)],], aes(id, value)) + 
  facet_wrap(~streetlight) + geom_bar(stat="identity", width=.3, position = "dodge") + 
  theme(axis.text.x = element_text(angle=45)) + scale_x_discrete(labels=c("Interpolation","Kalman","LOCF","MA","Mean",
                            "Random")) + labs(y="RMSE (A)",x="na_seadec approach", title="RMSE for Load current using na_seadec")

# Compute statistics for original and imputed data
na_seadec_sub <- gather(na_seadec_sub, id, value, c(4:52,56:62))
stats_na_seadec_sub <- na_seadec_sub %>% group_by(streetlight, id) %>%
  summarise(mean = mean(value, na.rm=TRUE), median = median(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE),
            skew = skewness(value, na.rm=TRUE), kurt = kurtosis(value, na.rm=TRUE))
stats_na_seadec_sub <- as.data.frame(stats_na_seadec_sub)  
stats_na_seadec_sub <- stats_na_seadec_sub[complete.cases(stats_na_seadec_sub),]
stats_na_seadec_sub <- stats_na_seadec_sub %>% mutate(mean=round(mean,2), median=round(median,2),
                                                      sd=round(sd,2), skew=round(skew,2), kurt=round(kurt,2))
stats_na_seadec_sub <- gather(stats_na_seadec_sub, "variable", "value", 3:7)
ggplot(stats_na_seadec_sub[stats_na_seadec_sub$streetlight=="SL2" & stats_na_seadec_sub$variable=="sd",], 
       aes(id, abs(value))) + geom_bar(stat="identity", width=.3, position = "dodge")  + 
  theme(axis.text.x = element_text(angle=90))

# Plot data to check mapping
ggplot(na_seadec_sub[na_seadec_sub$streetlight=="SL1" & na_seadec_sub$id==unique(na_seadec_sub$id)[c(44,56)],], 
       aes(timestamp, value, color=id)) + geom_line() + theme(legend.position = "bottom")