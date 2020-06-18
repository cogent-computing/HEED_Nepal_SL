# Implement the chosen imputation technique

# Calculate socket and system load - remove from sl_analysis then

# Calculate light load - remove from sl_analysis then

# Light load: sl_all$Light_load_W <- ifelse(sl_all$Battery_watts_W<0, sl_all$Battery_watts_W, 0)

#System and socket load : sl_all$sysSocketLoad <- sl_all$Battery_watts_W - sl_all$Battery_power_W 
# sl_all$socketLoad <- sl_all$PV_DC.coupled_W - sl_all$Battery_power_W 
#SL1: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL2: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec, Jan, Feb; use sysSocketLoad for Mar
#SL3: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar
#SL4: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb and Mar
#SL5: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar - only Jul and Aug data
#SL6: use socketLoad for Jul, Aug, Sep, Oct, Nov, Dec; use sysSocketLoad for Jan, Feb, Mar - skip Nov and Dec
#SL7: use socketLoad for Jul, Aug, Sep, Oct, Nov; use sysSocketLoad for Dec, Jan, Feb, Mar

# Apply corrections
# Corrections applied previously were: removing -ve load values; considering only 95% values