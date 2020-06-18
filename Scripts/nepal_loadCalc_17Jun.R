#******************************************************************************************#
# This is the script for assessing how to calculate socket load using raw data             #
# Author: K Bhargava                                                                       #
# Last updated on: 17th June 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Read SL data between July 2019 to March 2020
sl_all <- read_csv(here("Data","sl_all_raw.csv"), col_names = TRUE)
sl_all <- as.data.frame(sl_all[,seq_along(sl_all)])
#******************************************************************************************#

#******************************************************************************************#
# Calculate socket load by taking the diff of battery watts/PV.DC-coupled and battery power
sl_all <- sl_all %>% mutate(sysSocketLoad=Solar.Charger.Battery.watts.W - System.overview.Battery.Power.W,
                            socketLoad=System.overview.PV...DC.coupled.W - System.overview.Battery.Power.W,
                            month=as.character(month(timestamp, label=TRUE, abbr=TRUE)), timeUse=hour(timestamp))

# Create separate subsets for assessing socket load values 
socket_all <- sl_all[!(is.na(sl_all$socketLoad)),] #calculating socket load using PV-DC
socket_clean <- socket_all[socket_all$socketLoad>=0,] # Remove -ve values
# Calculate the 95% lower and higher indices for socketLoad 
socket_calcCI <- function(sl, p) {
  sl_sub <- sl %>% group_by(streetlight, month, timeUse) %>%
    summarise(lo = mean(socketLoad, na.rm=TRUE) - p * (sd(socketLoad, na.rm = TRUE)),
              hi = mean(socketLoad, na.rm=TRUE) + p * (sd(socketLoad, na.rm=TRUE)))
  sl_sub <- as.data.frame(sl_sub)
  sl_sub <- sl_sub[complete.cases(sl_sub),]
}
socket_clean_95 <- socket_calcCI(socket_clean, 2)
# Apply the 95% rule to subset the data - socketLoad 
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
socket_all_clean_95 <- socket_all_clean_95 %>% 
  mutate(month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                       labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

sys_all <- sl_all[!(is.na(sl_all$sysSocketLoad)),] #calcultaing socket load using Battery W
sys_clean <- sys_all[sys_all$sysSocketLoad>=0,] # Remove -ve values
# Calculate the 95% lower and higher indices for sysSocketLoad 
calcCI <- function(sl, p) {
  sl_sub <- sl %>% group_by(streetlight, month, timeUse) %>%
    summarise(lo = mean(sysSocketLoad, na.rm=TRUE) - p * (sd(sysSocketLoad, na.rm = TRUE)),
              hi = mean(sysSocketLoad, na.rm=TRUE) + p * (sd(sysSocketLoad, na.rm=TRUE)))
  sl_sub <- as.data.frame(sl_sub)
  sl_sub <- sl_sub[complete.cases(sl_sub),]
}
sl_clean_95 <- calcCI(sys_clean, 2)
# Apply the 95% rule to subset the data - sysSocketLoad 
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
sl_all_clean_95 <- sl_all_clean_95 %>% 
  mutate(month2=factor(month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                      labels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")))

# Plotting socket load - using PV.DC-coupled
plotLoad <- function(df) {
  ggplot(df, aes(timeUse, socketLoad)) + geom_point() +
    labs(x="Time of day", y= "System and socket load")
}
# Plotting socket load - using Battery watts
plotRawLoad <- function(df) {
  ggplot(df, aes(timeUse, sysSocketLoad)) + geom_point() +
    labs(x="Time of day", y= "System and socket load")
}

#SL1: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL1", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL1") + theme(legend.position = "none")
ggsave(here("Plots","sl1_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL1", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL1") + theme(legend.position = "none")
ggsave(here("Plots","sl1_sc_batteryPower.png"))

#SL2: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb; use sysSocketLoad for Mar
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL2", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL2") + theme(legend.position = "none")
ggsave(here("Plots","sl2_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL2", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL2") + theme(legend.position = "none")
ggsave(here("Plots","sl2_sc_batteryPower.png"))

#SL3: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL3", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL3") + theme(legend.position = "none")
ggsave(here("Plots","sl3_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL3", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL3") + theme(legend.position = "none")
ggsave(here("Plots","sl3_sc_batteryPower.png"))

#SL4: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb and Mar
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL4", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL4") + theme(legend.position = "none")
ggsave(here("Plots","sl4_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL4", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL4") + theme(legend.position = "none")
ggsave(here("Plots","sl4_sc_batteryPower.png"))

#SL5: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb and Mar - only Jul and Aug data
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL5", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL5") + theme(legend.position = "none")
ggsave(here("Plots","sl5_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL5", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL5") + theme(legend.position = "none")
ggsave(here("Plots","sl5_sc_batteryPower.png"))

#SL6: use socketLoad for Jul, Aug, Sep, Oct; use sysSocketLoad for Jan, Feb, Mar - skip Nov and Dec
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL6", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL6") + theme(legend.position = "none")
ggsave(here("Plots","sl6_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL6", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL6") + theme(legend.position = "none")
ggsave(here("Plots","sl6_sc_batteryPower.png"))

#SL7: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb and Mar
plotLoad(socket_all_clean_95[socket_all_clean_95$streetlight=="SL7", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (PV-DC power) for SL7") + theme(legend.position = "none")
ggsave(here("Plots","sl7_pv-dc.png"))
plotRawLoad(sl_all_clean_95[sl_all_clean_95$streetlight=="SL7", ]) + facet_wrap(~month2) +
  labs(title=">0 and 95% system and socket load (Battery watts) for SL7") + theme(legend.position = "none")
ggsave(here("Plots","sl7_sc_batteryPower.png"))
#******************************************************************************************#