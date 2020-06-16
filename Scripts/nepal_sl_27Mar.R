

##Getting typical load current values for each light per month
typical_load_current <- sl_all %>%
  group_by(month, timeUse, streetlight) %>%
  summarise(current = mean(Load_current_A, na.rm=TRUE))
typical_load_current <- as.data.frame(typical_load_current)

plotTypical <- function(df) {
  df %>%
    ggplot(aes(x=as.numeric(timeUse), current, color=streetlight)) + 
    geom_line(aes(linetype=streetlight)) +
    geom_point(aes(shape=streetlight)) +
    scale_shape_manual(values=c(1,4,1,4,1,4,1)) +
    labs(y="Load current (A)",
         x = "Time of day (hours)" ,
         color="Streetlight", 
         shape="Streetlight",
         linetype="Streetlight") + 
    scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24),
                       labels=c("0","2","4","6","8","10","12","14","16","18","20","22","24"))
}

plotTypical(typical_load_current[typical_load_current$month=="Jul",]) +
  labs(title="Load current for Nepal SL in Jul 2019")

plotTypical(typical_load_current[typical_load_current$month=="Aug",]) +
  labs(title="Load current for Nepal SL in Aug 2019")

plotTypical(typical_load_current[typical_load_current$month=="Sep",]) +
  labs(title="Load current for Nepal SL in Sep 2019")

plotTypical(typical_load_current[typical_load_current$month=="Oct",]) +
  labs(title="Load current for Nepal SL in Oct 2019")

plotTypical(typical_load_current[typical_load_current$month=="Nov",]) +
  labs(title="Load current for Nepal SL in Nov 2019")

plotTypical(typical_load_current[typical_load_current$month=="Dec",]) +
  labs(title="Load current for Nepal SL in Dec 2019")

plotTypical(typical_load_current[typical_load_current$month=="Jan",]) +
  labs(title="Load current for Nepal SL in Jan 2020")

plotTypical(typical_load_current[typical_load_current$month=="Feb",]) +
  labs(title="Load current for Nepal SL in Feb 2020")

plotTypical(typical_load_current[typical_load_current$month=="Mar",]) +
  labs(title="Load current for Nepal SL in Mar 2020")

##Load cut offs - ignore data when load current==0 and time is less than 6am or greater than 6pm
#Remove rows where the light load is 0 between 18:00 and 6:00
sl_all$time <- strftime(sl_all$timestamp, "%H:%M:%S", tz="GMT")
#systemData <- sl_all[is.na(sl_all$Load_current_A) | sl_all$Load_current_A!=0 | 
#                       (sl_all$Load_current_A==0 & (sl_all$time>"06:00:00" & 
#                                                            sl_all$time<"18:00:00")), ]
systemData <- sl_all
#Remove Load current and time value
systemData <- systemData[,-c(6,14)]

####Calculate system and socket load and remove -ve values
systemData$sysSocketLoad <- -1*(systemData$Battery_power_W - systemData$Battery_watts_W)
system_sub <- systemData[,c(1,9,10,11,12,13)]
system_sub <- system_sub[system_sub$streetlight=="SL1" & system_sub$month=="Jan",]
system_sub <- distinct(system_sub)
system_sub <- system_sub[!is.na(system_sub$sysSocketLoad),]
neg_values <- length(system_sub$sysSocketLoad[system_sub$sysSocketLoad<0])
plotRawLoad <- function(df) {
  df %>%
    ggplot(aes(timeUse, sysSocketLoad, color=month)) +
    geom_point() +
    labs(x="Time of day (00-23 hours)",
         y= "System and socket load")
}
system_sub <- system_sub[system_sub$sysSocketLoad>=0,]
plotRawLoad(system_sub)
test <- system_sub[system_sub$date<"2020-01-17" | system_sub$date>"2020-01-19", ]
plotRawLoad(test)
test2 <- test[test$sysSocketLoad>10 & test$timeUse<"06",]

#Remove values when the light turns off - ~6:40-7
test$time <- strftime(test$timestamp, "%H:%M:%S", tz="GMT")
test2 <- test[!(test$time>="06:20:00" & test$time<="07:10:00"),]
plotRawLoad(test2)

#Calculate 95% CI - Sl1 in Jan
system_ci <- system_sub %>%
  group_by(timeUse) %>%
  summarise(lo = mean(sysSocketLoad) - 2 * (sd(sysSocketLoad)),
            hi = mean(sysSocketLoad) + 2 * (sd(sysSocketLoad)))
test_ci <- data.frame()
for(i in 1:length(system_ci$timeUse)) {
  df <- system_sub[system_sub$timeUse==system_ci$timeUse[i], ]
  df <- df[df$sysSocketLoad>=system_ci$lo[i] & df$sysSocketLoad<=system_ci$hi[i],]
  test_ci <- rbind(test_ci, df)
}

plotRawLoad(test_ci)

system_sub[system_sub$timeUse=="09",] %>%
  ggplot(aes(sysSocketLoad)) +
  geom_histogram(aes(y = ..density..),binwidth=1,colour="black", fill="white") +
  stat_function(fun = dnorm, args = list(mean = mean(system_sub$sysSocketLoad[system_sub$timeUse=="00"]), 
                                         sd = sd(system_sub$sysSocketLoad[system_sub$timeUse=="00"]))) +
  geom_vline(xintercept = mean(system_sub$sysSocketLoad[system_sub$timeUse=="00"]), color="blue") +
  geom_vline(xintercept = mean(system_sub$sysSocketLoad[system_sub$timeUse=="00"]) -
               2 * sd(system_sub$sysSocketLoad[system_sub$timeUse=="00"]), color="green") +
  geom_vline(xintercept = mean(system_sub$sysSocketLoad[system_sub$timeUse=="00"]) + 
               2 * sd(system_sub$sysSocketLoad[system_sub$timeUse=="00"]), color="green")


plotRawLoad(systemData[systemData$streetlight=="SL1" & systemData$month=="Jan", ]) +
  labs(title="System and socket load for SL1")
plotRawLoad(systemData[systemData$streetlight=="SL2", ]) +
  labs(title="System and socket load for SL2")
plotRawLoad(systemData[systemData$streetlight=="SL3", ]) +
  labs(title="System and socket load for SL3")
plotRawLoad(systemData[systemData$streetlight=="SL4", ]) +
  labs(title="System and socket load for SL4")
plotRawLoad(systemData[systemData$streetlight=="SL5", ]) +
  labs(title="System and socket load for SL5")
plotRawLoad(systemData[systemData$streetlight=="SL6", ]) +
  labs(title="System and socket load for SL6")
plotRawLoad(systemData[systemData$streetlight=="SL7", ]) +
  labs(title="System and socket load for SL7")

plotRawLoad <- function(df) {
  df %>%
    ggplot(aes(timeUse, totalLoad)) +
    geom_point() +
    labs(x="Time of day (00-23 hours)",
         y= "Total load")
}

###Remove sysSocketLoad values that are <0

systemData <- systemData[,-14] #Remove total load
#Gather data to get hourly means
systemData <- gather(systemData, "id", "value", c(2:8,13))

hourlyMeans <- function(system_gather) {
  #Hourly means can be calculated for AC consumption and PV power
  system_hourly <- system_gather[system_gather$id=="PV_DC.coupled_W" | 
                                   system_gather$id=="PV_power_W" |
                                   system_gather$id=="Battery_power_W" |
                                   system_gather$id=="Battery_watts_W" |
                                   system_gather$id=="sysSocketLoad" | 
                                   system_gather$id=="State_of_charge", ] %>%
    group_by(streetlight, date, timeUse, id) %>%
    summarise(value=mean(value,na.rm = TRUE))
  system_hourly <- as.data.frame(system_hourly)
  
  #Calculate hourly values for discharged and charged energy by taking hourly differences
  battery_charge <- system_gather[system_gather$id=="Charged_energy_kWh",]
  battery_charge <- battery_charge[complete.cases(battery_charge), ]
  #Extract hourly values by taking the last value for each hour 
  battery_charge_hours <- battery_charge %>%
    group_by(streetlight, date, timeUse, id) %>%
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
    group_by(streetlight, date, timeUse, id) %>%
    summarise(value = value[length(na.omit(value))])
  battery_discharge_hours <- as.data.frame(battery_discharge_hours)
  a <- diff(battery_discharge_hours$value)
  battery_discharge_hours <- battery_discharge_hours[-1,]
  battery_discharge_hours$value <- a
  battery_discharge_hours$value <- battery_discharge_hours$value * 1000.0 #W
  system_hourly <- rbind(system_hourly, battery_discharge_hours)
}

system_hourly <- hourlyMeans(systemData)
sl_write <- spread(system_hourly, id, value)
sl_write$timestamp <- as.POSIXct(paste(paste(sl_write$date, sl_write$timeUse), ":00:00",sep=""),
                                 format="%Y-%m-%d %H:%M:%S", tz="GMT")

#Replace NA values for charged and discharged energy with 0
sl_write$Charged_energy_kWh[is.na(sl_write$Charged_energy_kWh)] <- 0
sl_write$Discharged_energy_kWh[is.na(sl_write$Discharged_energy_kWh)] <- 0

sl_write <- sl_write[,-c(2,3)] #Remove date and timeUse
sl_write <- sl_write[, c(10,1:9)]
colnames(sl_write) <- c("timestamp","streetlight","Battery_power_W",
                        "Battery_watts_W", "Charged_energy_W",
                        "Discharged_energy_W", "PV_DC_coupled_W",
                        "PV_power_W", "State_of_charge_%","System_socket_load_W")
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
write.csv(sl_write, file="SL_all_jul_mar.csv", row.names=FALSE)

###############################################################################
###Read in the system file
setwd("~/OneDrive - Coventry University/HEED_analysis/Nepal Streetlights/Data/")
sl_all <- read.csv("SL_all_jul_mar.csv", header=TRUE, stringsAsFactors = FALSE)
sl_all$timestamp <- as.POSIXct(sl_all$timestamp, tz="GMT", origin="1970-01-01")
sl_all$date <- date(sl_all$timestamp)
sl_all$timeUse <- format(sl_all$timestamp, format='%H')

###################################################################################################
#Hourly socket and system consumption for streetlight (1-7) in (month). 
#system and socket loads = Battery power - pv dc-coupled
sl_all$day <- weekdays(sl_all$date, abbr=TRUE)
sl_all$month <- as.character(month(sl_all$date, label=TRUE, abbr=TRUE))

sl_all_weday <- sl_all[sl_all$day=="Sat" | sl_all$day=="Sun", c(2,10,11,12,13,14)]
sl_all_wday <- sl_all[sl_all$day!="Sat" & sl_all$day!="Sun", c(2,10,11,12,13,14)]

plotMeans <- function(df,lim1,lim2) {
  df %>%
    ggplot(aes(x=timeUse, y=System_socket_load_W)) +
    geom_boxplot(aes(fill=timeUse))  +
    labs(y="Power consumption (W)",
         x = "Hour of day") + 
    theme(plot.title = element_text(size=10), legend.position = "none") +
    scale_y_continuous(limits = c(lim1,lim2))
}

####Plots for SL1
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Jul",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jul'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Jul",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Aug",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Aug",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Sep",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Sep",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Oct",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Oct",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Nov",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Nov",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Dec",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Dec",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Jan",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Jan",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Jan'20")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Feb",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Feb",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL1" & sl_all_wday$month=="Mar",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL1" & sl_all_weday$month=="Mar",], 0, 30)
p + labs(title="Mean hourly system and socket load at SL1 on a weekend day in Mar'20") 

####Plots for SL2
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Jul'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Aug'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Nov'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Jan'20")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL2" & sl_all_wday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL2" & sl_all_weday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL2 on a weekend day in Mar'20") 

####Plots for SL3
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Jul'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Jan'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Feb'20")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL3" & sl_all_wday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL3" & sl_all_weday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL3 on a weekend day in Mar'20") 

###Plots for SL4
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Jul'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Jul'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Aug'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Oct'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Nov",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Dec'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Dec",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Jan'20")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL4" & sl_all_wday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL4" & sl_all_weday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL4 on a weekend day in Mar'20")

###Plots for SL5
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL5" & sl_all_wday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL5 on a weekday in Jul'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL5" & sl_all_weday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL5 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL5" & sl_all_wday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL5 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL5" & sl_all_weday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL5 on a weekend day in Aug'19") 

####Plots for SL6
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Jul'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Jul",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Jul'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Aug",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Sep",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Oct",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Nov",], 0,50)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Nov",], 0,50)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Dec",], 0,50)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Dec'19") 

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Dec",], 0,50)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Dec'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Jan",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Jan'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Feb",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL6" & sl_all_wday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL6" & sl_all_weday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL6 on a weekend day in Mar'20")

####Plots for SL7
p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Jul",], 00,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Jul'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Jul",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Jul'19")

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Aug",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Aug'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Aug",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Aug'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Sep",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Sep'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Sep",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Sep'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Oct",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Oct'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Oct",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Oct'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Nov",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Nov'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Nov",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Nov'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Dec",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Dec'19")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Dec",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Dec'19") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Jan",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Jan'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Jan",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Jan'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Feb",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Feb'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Feb",], 0,200)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Feb'20") 

p <- plotMeans(sl_all_wday[sl_all_wday$streetlight=="SL7" & sl_all_wday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL7 on a weekday in Mar'20")

p <- plotMeans(sl_all_weday[sl_all_weday$streetlight=="SL7" & sl_all_weday$month=="Mar",], 0,20)
p + labs(title="Mean hourly system and socket load at SL7 on a weekend day in Mar'20")

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

