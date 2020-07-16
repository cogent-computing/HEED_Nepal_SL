#******************************************************************************************#
# This is the script to prepare data for upload on Zenodo                                  #
# Author: K Bhargava                                                                       #
# Last updated on: 16th July 2020                                                          #
#******************************************************************************************#

#******************************************************************************************#
# Importing libraries
library(tidyverse)
library(lubridate)
library(here)
#******************************************************************************************#

#******************************************************************************************#
# Set working directory 
filepath <- "Data/Zenodo"
#******************************************************************************************#

#******************************************************************************************#
# Read SL data between July 2019 and March 2020, subset data and re arrange columns
sl_nepal <- read_csv(here(filepath,"sl_all_raw_jul19_mar20.csv"), col_names = TRUE)
sl_nepal <- as.data.frame(sl_nepal[,seq_along(sl_nepal)])
sl_nepal <- sl_nepal %>% mutate(date=date(timestamp)) 

# Remove extra columns
sl_nepal <- sl_nepal[,-c(5,7)]
# Subset data till 31st March 2020
sl_nepal <- sl_nepal[sl_nepal$date<="2020-03-31",]
sl_nepal <- distinct(sl_nepal)
# Remove date
sl_nepal <- sl_nepal[,-7]
# Rename columns
colnames(sl_nepal) <- c("Timestamp (Asia/Kathmandu)", "Streetlight", "Battery Monitor State of Charge (%)",
                      "Solar Charger Battery Power (W)", "System Overview Battery Power (W)", 
                      "Solar Charger PV Power (W)")
# Re-arrange columns
sl_all <- sl_nepal[,c(1:4,6,5)]
#******************************************************************************************#

#******************************************************************************************#
# Save data for each SL
filepath <- "Data/Zenodo/Nepal SL"

sl <- sl_all[sl_all$Streetlight=="SL1", -2] #SL1
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL1.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL2", -2] #SL2
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL2.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL3", -2] #SL3
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL3.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL4", -2] #SL4
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL4.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL5", -2] #SL5
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL5.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL6", -2] #SL6
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL6.csv"), row.names=FALSE)

sl <- sl_all[sl_all$Streetlight=="SL7", -2] #SL7
sl <- sl[order(sl$Timestamp),]
write.csv(sl, file=here(filepath,"Nepal_SL7.csv"), row.names=FALSE)
#******************************************************************************************#