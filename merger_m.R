rm(list = ls())

library(tidyverse)
library(zoo)

################################################################################
################################# ENERGY DATA ##################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_energy")
de_raw = read_csv("25_energy.csv")

de = de_raw %>%
  mutate(YearMon = as.yearmon(Date),
         treat = case_when(
           YearMon >= as.yearmon("2022-06-01") ~ 1,
           TRUE ~ 0 
         )) %>%
  group_by(Country, YearMon) %>%
  mutate(
    Load    = sum(Load, na.rm = TRUE),
    NetIM   = sum(`Import Balance`, na.rm = TRUE),
    Nuclear = sum(Nuclear, na.rm = TRUE),
    NRenew  = sum(`Non-Renewable`, na.rm = TRUE),
    Renew   = sum(Renewable, na.rm = TRUE),
    DAPrice = mean(`Day Ahead Auction`, na.rm = TRUE) ) %>%
  select(YearMon, Country, Region, Load, NetIM, 
         Nuclear, NRenew, Renew, DAPrice, treat) %>%
  distinct(.keep_all = TRUE) 


################################################################################
################################### TEMP DATA ##################################
################################################################################

#setwd("/Users/Miguel/Desktop/energy_price/data/data_temp")
#
#avgtemp_raw = read_csv("25_avgtemp.csv")
#avgtemp = avgtemp_raw %>%
#  filter(Alert == "OK") %>%
#  mutate(YearMon = as.yearmon(Date)) %>%
#  group_by(Country, YearMon) %>%
#  mutate(AvgTemp = mean(Temperature *0.1)) %>%
#  select(YearMon, Country, AvgTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#maxtemp_raw = read_csv("25_maxtemp.csv")
#maxtemp = maxtemp_raw %>% 
#  filter(Alert == "OK") %>%
#  mutate(YearMon = as.yearmon(Date)) %>%
#  group_by(Country, YearMon) %>%
#  mutate(MaxTemp = mean(Temperature*0.1)) %>%
#  select(YearMon, Country, MaxTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#mintemp_raw = read_csv("25_mintemp.csv")
#mintemp = mintemp_raw %>% 
#  filter(Alert == "OK") %>%
#  mutate(YearMon = as.yearmon(Date)) %>%
#  group_by(Country, YearMon) %>% 
#  mutate(MinTemp = mean(Temperature*0.1)) %>%
#  select(YearMon, Country, MinTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#temp_lt = read_csv("LT_temp.csv") %>%
#  mutate(YearMon = as.yearmon(datetime),
#         Country = "LT") %>%
#  group_by(Country, YearMon) %>%
#  mutate(AvgTemp = mean(temp, na.rm = T),
#         MaxTemp = mean(tempmax, na.rm = T),
#         MinTemp = mean(tempmin, na.rm = T)) %>%
#  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#temp_gr = read_csv("GR_temp.csv") %>%
#  mutate(YearMon = as.yearmon(datetime),
#         Country = "GR")  %>%
#  group_by(Country, YearMon) %>%
#  mutate(AvgTemp = mean(temp, na.rm = T),
#         MaxTemp = mean(tempmax, na.rm = T),
#         MinTemp = mean(tempmin, na.rm = T)) %>%
#  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#temp_bg = read_csv("BG_temp.csv") %>%
#  mutate(YearMon = as.yearmon(datetime),
#         Country = "BG") %>%
#  group_by(Country, YearMon) %>%
#  mutate(AvgTemp = mean(temp, na.rm = T),
#         MaxTemp = mean(tempmax, na.rm = T),
#         MinTemp = mean(tempmin, na.rm = T)) %>%
#  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
#  distinct(.keep_all = TRUE) 
#
#
#temp_data = merge(avgtemp,
#                  merge(maxtemp, mintemp,  
#                        by = c("YearMon", "Country"), all = TRUE),
#                  by = c("YearMon", "Country"), all = TRUE )
#
#temp_data_all = rbind(temp_data, temp_gr, temp_lt, temp_bg)
#
#dt = temp_data_all %>% 
#  mutate(
#    winter = case_when(
#      format(YearMon, format = "%m") == "09" ~ 1,
#      format(YearMon, format = "%m") == "10" ~ 1,
#      format(YearMon, format = "%m") == "11" ~ 1,
#      format(YearMon, format = "%m") == "12" ~ 1,
#      format(YearMon, format = "%m") == "01" ~ 1,
#      format(YearMon, format = "%m") == "02" ~ 1,
#      TRUE ~ 0 
#    ),
#    summer = case_when(
#      format(YearMon, format = "%m") == "03" ~ 1,
#      format(YearMon, format = "%m") == "04" ~ 1,
#      format(YearMon, format = "%m") == "05" ~ 1,
#      format(YearMon, format = "%m") == "06" ~ 1,
#      format(YearMon, format = "%m") == "07" ~ 1,
#      format(YearMon, format = "%m") == "08" ~ 1,
#      TRUE ~ 0
#    ))
#
#agg_data1 = merge(de, dt, by = c("YearMon", "Country"),  all = TRUE )
#
##agg_data1  %>%
##  group_by(YearMon, Country) %>%
##  summarise(n = n(), .groups = "drop") %>%
##  filter(n > 1L)
#
################################################################################
############################## POPULATION DATA #################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_pop")
dp_raw = read_csv("data_pop.csv")

dp = dp_raw %>% 
  filter(TIME_PERIOD == 2021 &
           geo != "IE" &
           geo != "CH") %>% 
  mutate(Country = geo,
         Population = OBS_VALUE) %>%
  mutate(Country = case_when(
    Country == "EL" ~ "GR",
    TRUE ~ Country
  )) %>%
  select(Country, Population)

#agg_data2 = merge(agg_data1, dp, by = "Country", all = TRUE)
agg_data2 = merge(de, dp, by = "Country", all = TRUE)

agg_data2  %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

################################################################################
############################# DEGREE DAYS DATA #################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_temp")

hdd = read.csv("hdd.csv") %>%
  mutate(Country = geo,
         YearMon = as.yearmon(TIME_PERIOD),
         HDD = OBS_VALUE,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  filter(Country != "LU" 
         & Country != "IE"
         & YearMon >= as.yearmon("Jan 2021")
         & YearMon <= as.yearmon("Dec 2021")) %>%
  group_by(Country) %>%
  summarise(mean_HDD = mean(HDD, na.rm = T),
            sum_HDD  = sum(HDD, na.rm = T)) %>%
  mutate(YearMon = as.yearmon("Dec 2021"))

cdd = read.csv("cdd.csv") %>%
  mutate(Country = geo,
         YearMon = as.yearmon(TIME_PERIOD),
         CDD = OBS_VALUE,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  filter(Country != "LU" 
         & Country != "IE"
         & YearMon >= as.yearmon("Jan 2021")
         & YearMon <= as.yearmon("Dec 2021")) %>%
  group_by(Country) %>%
  summarise(mean_CDD = mean(CDD, na.rm = T),
            sum_CDD  = sum(CDD, na.rm = T)) %>%
  mutate(YearMon = as.yearmon("Dec 2021"))

ddd = merge(hdd, cdd, by = c("Country", "YearMon"))

agg_data3 = merge(agg_data2, ddd, by = c("Country", "YearMon"), all = TRUE)

agg_data3  %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

################################################################################
################################### MIDX DATA ##################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_midx")

didx = read_csv("market_idxs.csv") %>%
  mutate(Country = geo,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
           ),
         indic = case_when( 
           indic_nrgm == "ERTL_LG" ~ "Retail",
           indic_nrgm == "LCMPY_EG" ~ "Generation",
           indic_nrgm == "LCMPY_IECAP" ~ "Capacity"
           )
         ) %>%
  mutate(YearMon = as.yearmon("2020-01")) %>%
  select(Country, YearMon, indic, OBS_VALUE) %>%
  pivot_wider(names_from = indic, 
              values_from = OBS_VALUE)

agg_data4 = merge(agg_data3, didx, by = c("Country", "YearMon"), all = TRUE)

agg_data4  %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

################################################################################
################################### CPI DATA ###################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_cpi")

dcpi = read.csv("CPI_index.csv") %>%
#dcpi = read.csv("CPI_mmrc.csv") %>%
#dcpi = read.csv("CPI_marc.csv") %>%
  mutate(YearMon = as.yearmon(TIME_PERIOD),
         Country = geo,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country),
         #coicop = case_when(
         #  coicop == "CP00" ~ "cpi_all_items",
         #  coicop == "CP01" ~ "cpi_food_non_alcohol",
         #  coicop == "CP02" ~ "cpi_alcohol_tobacco",
         #  coicop == "CP03" ~ "cpi_clothing",
         #  coicop == "CP04" ~ "cpi_housing_energy",
         #  coicop == "CP041" ~ "cpi_rental_housing",
         #  coicop == "CP044" ~ "cpi_dwelling_services",
         #  coicop == "CP045" ~ "cpi_electricity_gas_fuels",
         #  coicop == "CP0451" ~ "cpi_electricity",
         #  coicop == "CP0452" ~ "cpi_gas",
         #  coicop == "CP04521" ~ "cpi_natural_gas",
         #  coicop == "CP04522" ~ "cpi_liquefied_hydrocarbons",
         #  coicop == "CP0453" ~ "cpi_liquid_fuels",
         #  coicop == "CP0454" ~ "cpi_solid_fuels",
         #  coicop == "CP04541" ~ "cpi_coal",
         #  coicop == "CP04549" ~ "cpi_other_solid_fuels",
         #  coicop == "CP0455" ~ "cpi_heat_energy",
         #  coicop == "CP05" ~ "cpi_household_equipment_maintenance",
         #  coicop == "CP06" ~ "cpi_health",
         #  coicop == "CP07" ~ "cpi_transport",
         #  coicop == "CP071" ~ "cpi_purchase_vehicles",
         #  coicop == "CP072" ~ "cpi_personal_transport_equipment",
         #  coicop == "CP073" ~ "cpi_transport_services",
         #  coicop == "CP08" ~ "cpi_communications",
         #  coicop == "CP09" ~ "cpi_recreation_culture",
         #  coicop == "CP10" ~ "cpi_education",
         #  coicop == "CP11" ~ "cpi_restaurants_hotels",
         #  coicop == "CP12" ~ "cpi_miscellaneous_goods_services",
         #  coicop == "CP121" ~ "cpi_personal_care",
         #  coicop == "CP123" ~ "cpi_personal_effects",
         #  coicop == "CP124" ~ "cpi_social_protection",
         #  coicop == "CP125" ~ "cpi_insurance",
         #  coicop == "CP126" ~ "cpi_financial_services",
         #  coicop == "CP127" ~ "cpi_oter_services",
         #  coicop == "ELC_GAS" ~ "cpi_electriicty_gas_solid_fuels_heat_energy",
         #  coicop == "FUEL" ~ "cpi_all_fuels",
         #  coicop == "GD" ~ "cpi_goods_only",
         #  coicop == "IGD" ~ "cpi_industrial_goods",
         #  coicop == "IGD_NNRG" ~ "cpi_non_energy_industrial_goods",
         #  coicop == "NRG" ~ "cpi_energy",
         #  coicop == "SERV" ~ "cpi_services_only",
         #  coicop == "TOT_X_NRG" ~ "cpi_no_energy")
         ) %>%
  select(YearMon, Country, OBS_VALUE, coicop) %>%
  pivot_wider(names_from = coicop,
              values_from = OBS_VALUE)

agg_data5 = merge(agg_data4, dcpi, by = c("Country", "YearMon"), all = TRUE)

agg_data5  %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

################################################################################
################################### GDP DATA ###################################
################################################################################

setwd("/Users/Miguel/Desktop/master_thesis/data/data_gdp")

dgdp = read_csv("data_gdp.csv") %>% 
  mutate(YearMon = as.yearmon(Date)) %>%
  select(YearMon, Country, GDP)

agg_data_all = merge(agg_data5, dgdp, by = c("Country", "YearMon"), all = TRUE)

### CHECK FOR DUPLICATES 
agg_data_all %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

setwd("/Users/Miguel/Desktop/master_thesis")
write_csv(agg_data_all, "25_yearmon.csv")
   



