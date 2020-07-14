#******************************************************************************************#
# This is the script for analysing Nepal SL data                                           #
# Author: K Bhargava                                                                       #
# Last updated on: 6th July 2020                                                           #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(wesanderson)
library(extrafont)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Define macros - theme for all plots - ADD times new roman
font_import()
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
# Read corrected SL data and energy excess data
na_seadec_correctedData <- read.csv(here(filepath,"na_seadec_correctedData.csv"), header=TRUE, stringsAsFactors=FALSE)
na_seadec_correctedData <- na_seadec_correctedData %>% 
  mutate(date=as.Date(date),timestamp=as.POSIXct(timestamp, tz="GMT", origin="1970-01-01"),
         month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                       labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))
energy_excess <- read.csv(here(filepath,"Excess elc at sockets Nepal 2.csv"), header=TRUE, stringsAsFactors=FALSE) 
energy_excess <- energy_excess %>% 
  mutate(Time=as.POSIXct(Time, tz="GMT", origin="1970-01-01", format="%d/%m/%Y %H:%M"),
         timeUse = hour(Time))
#******************************************************************************************#

#******************************************************************************************#
# Get full data days from original data - all variables needed
system_sub_original <- na_seadec_correctedData[,c(1:3,5,7,9,11,17)]
system_sub_original <- system_sub_original[complete.cases(system_sub_original),]
onHours <- system_sub_original %>% group_by(streetlight, month2, date) %>% 
  summarise(hours=length(Solar.Charger.PV.power.W_original))
onHours <- as.data.frame(onHours[onHours$hours==24,])
full_days <- onHours %>% group_by(streetlight, month2) %>% summarise(days=length(date)) 
full_days <- as.data.frame(full_days)
write.csv(full_days, file=here(filepath,"full_days_all_data_nepal.csv"), row.names=FALSE)
#******************************************************************************************#

#******************************************************************************************#
# Get typical day values for each SL
# Subset data to get SL, Time, Potential PV, SoC, Actual PV power, Actual Socket load, 
# Positive actual Solar battery power (E_a), Positive and negative actual battery power,
# Actual light load
system_sub_interpolation <- na_seadec_correctedData[,c(1,3,15,14,4,32,33,36,37,39,40,41)]
system_sub_original <- na_seadec_correctedData[,c(1,3,15,14,5,32,33,36,37,39,40,41)]

system_sub_original <- system_sub_original %>% 
  mutate(load = abs(Actual.Socket.load.W_interpolation) + 
           abs(Actual.Light.load.W_interpolation),
         loss = Potential.PV.power.W - 
           Postive.Actual.Solar.Charger.Battery.Power.W_interpolation,
         State.of.Charge.W_interpolation=State.of.Charge.W_interpolation*100/1954)
system_sub_original <- system_sub_original[,-c(8,12)] # Remove socket and light load
colnames(system_sub_original) <- c("streetlight","timeUse","month","E_p", "BM_SoC","SoC",
                                   "PV","E_a","B_cp","B_dp","E_load","L_c")

# Calculate Total load (actual AC + actual light), capture loss (potential-E_a)
system_sub_interpolation <- system_sub_interpolation %>% 
  mutate(load = abs(Actual.Socket.load.W_interpolation) + 
           abs(Actual.Light.load.W_interpolation),
         loss = Potential.PV.power.W - 
           Postive.Actual.Solar.Charger.Battery.Power.W_interpolation,
         State.of.Charge.W_interpolation=State.of.Charge.W_interpolation*100/1954)
system_sub_interpolation <- system_sub_interpolation[,-c(8,12)] # Remove socket & light load
colnames(system_sub_interpolation) <- c("streetlight","timeUse","month","E_p", "BM_SoC","SoC",
                                        "PV","E_a","B_cp","B_dp","E_load","L_c")

# Remove months July to Sep for SL7
system_sub_original <- system_sub_original[!(system_sub_original$streetlight=="SL7" & 
                      (system_sub_original$month=="Jul" | system_sub_original$month=="Aug" |
                             system_sub_original$month=="Sep")),]
system_sub_interpolation <- 
  system_sub_interpolation[!(system_sub_interpolation$streetlight=="SL7" & 
  (system_sub_interpolation$month=="Jul" | system_sub_interpolation$month=="Aug" |
                                  system_sub_interpolation$month=="Sep")),]
# Calculate typical values for each SL
system_sub_original <- gather(system_sub_original, id, value, 4:12)
system_typical_original <- system_sub_original %>% 
  group_by(streetlight, timeUse, id) %>% summarise(value=mean(value, na.rm=TRUE))
system_typical_original <- as.data.frame(system_typical_original)
system_typical_original <- spread(system_typical_original, id, value)

system_sub_interpolation <- gather(system_sub_interpolation, id, value, 4:12)
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
    labs(y="Power (kW)", x = "Time of day", colour="", linetype="") +
    scale_x_continuous(breaks=seq(0,24,by=2)) + THEME
}
# "Actual typical day profile for Nepal SL1 between Jul 2019 and Mar 2020"
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL1",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl1_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL2",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl2_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL3",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl3_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL4",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl4_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL5",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl5_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL6",]) +
  geom_line(aes(y = BM_SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl6_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
plotTypical(system_typical_interpolation[system_typical_interpolation$streetlight=="SL7",]) +
  geom_line(aes(y = SoC/400, color = "SoC",linetype="SoC")) 
ggsave(here(plot_dir,"typical_day_sl7_imputed_interpolation.pdf"), width = 8, height = 6, units = "cm")
#******************************************************************************************#

#*****************************************************************************************#
# Box plots for Socket consumption
plotACLoad <- function(df) {
  ggplot(df, aes(as.factor(timeUse), Actual.Socket.load.W_interpolation/1000.0)) + 
    geom_boxplot() + labs(x="Time of day", y="Socket consumption (kW)") + THEME + 
    scale_x_discrete(breaks=seq(0,24,by=2))
}
# labs(title="Hourly socket consumption at Nepal SL1 between Jul 2019 and Mar 2020")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL1",]) 
ggsave(here(plot_dir,"acLoad_sl1_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL2",]) 
ggsave(here(plot_dir,"acLoad_sl2_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL3",]) 
ggsave(here(plot_dir,"acLoad_sl3_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL4",]) 
ggsave(here(plot_dir,"acLoad_sl4_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL5",])
ggsave(here(plot_dir,"acLoad_sl5_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL6",]) 
ggsave(here(plot_dir,"acLoad_sl6_imputed.pdf"), width = 8, height = 6, units = "cm")
plotACLoad(na_seadec_correctedData[na_seadec_correctedData$streetlight=="SL7" & 
            na_seadec_correctedData$date>="2019-10-01",]) 
ggsave(here(plot_dir,"acLoad_sl7_imputed.pdf"), width = 8, height = 6, units = "cm")

# Plot across all select SL - except SL5 and SL7
# labs(title="Hourly socket consumption at Nepal SL1-4 and SL6 between Jul 2019 and Mar 2020")
plotACLoad(na_seadec_correctedData[!(na_seadec_correctedData$streetlight=="SL5" | 
                                     na_seadec_correctedData$streetlight=="SL7"),]) + THEME 
ggsave(here(plot_dir,"acLoad_sl_all_nepal.pdf"), width = 8, height = 6, units = "cm")

# Create an avg SL for each date and hour considering SL1-4 and SL6
avgSL <- na_seadec_correctedData[!(na_seadec_correctedData$streetlight=="SL5" | 
                                     na_seadec_correctedData$streetlight=="SL7"),] %>% 
  group_by(date, timeUse) %>%
  summarise(Actual.Socket.load.W_interpolation=mean(Actual.Socket.load.W_interpolation))
avgSL <- as.data.frame(avgSL)
# labs(title="Hourly socket consumption at average Nepal SL between Jul 2019 and Mar 2020")
plotACLoad(avgSL) + THEME 
ggsave(here(plot_dir,"acLoad_avg_sl_nepal.pdf"), width = 8, height = 6, units = "cm")

# Avg socket load value across SL1-4 and SL6 against timeUse for all months
avgLoad <- na_seadec_correctedData[!(na_seadec_correctedData$streetlight=="SL5" | 
                                       na_seadec_correctedData$streetlight=="SL7"),] %>% 
  group_by(timeUse) %>% 
  summarise(Socket.load.kW = mean(Actual.Socket.load.W_interpolation)/1000.0)
write.csv(avgLoad, file=here(filepath,"avg_hourly_socketLoad_nepal.csv"), row.names=FALSE)

# PLot energy excess data
# title="Excess electricity at Nepal streetlight sockets between Jul 2019 and Mar 2020"
ggplot(energy_excess, aes(as.factor(timeUse), Excess.Electricity.at.Socket.kW)) +
  geom_boxplot() + labs(x="Time of day", y="Excess electricity (kW)") +
  THEME + scale_y_continuous(breaks=seq(0,0.25,0.05)) + scale_x_discrete(breaks=seq(0,24,by=2))
ggsave(here(plot_dir,"excess_energy_nepal_sl.pdf"), width = 8, height = 6, units = "cm")
#*****************************************************************************************#

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
# Converting power from W to kW
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