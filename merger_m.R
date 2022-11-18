rm(list = ls())

library(tidyverse)
library(zoo)

################################################################################
################################# ENERGY DATA ##################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_energy")
de_raw = read_csv("28_energy.csv")

de = de_raw %>%
  mutate(treat = case_when( 
    Date >= as.Date("2022-05-01") ~ 0,
    TRUE ~ 1 ) ) %>% 
  mutate(YearMon = as.yearmon(Date)) %>%
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

setwd("/Users/Miguel/Desktop/energy_price/data/data_temp")

avgtemp_raw = read_csv("25_avgtemp.csv")
avgtemp = avgtemp_raw %>%
  filter(Alert == "OK") %>%
  mutate(YearMon = as.yearmon(Date)) %>%
  group_by(Country, YearMon) %>%
  mutate(AvgTemp = mean(Temperature *0.1)) %>%
  select(YearMon, Country, AvgTemp) %>%
  distinct(.keep_all = TRUE) 

maxtemp_raw = read_csv("25_maxtemp.csv")
maxtemp = maxtemp_raw %>% 
  filter(Alert == "OK") %>%
  mutate(YearMon = as.yearmon(Date)) %>%
  group_by(Country, YearMon) %>%
  mutate(MaxTemp = mean(Temperature*0.1)) %>%
  select(YearMon, Country, MaxTemp) %>%
  distinct(.keep_all = TRUE) 

mintemp_raw = read_csv("25_mintemp.csv")
mintemp = mintemp_raw %>% 
  filter(Alert == "OK") %>%
  mutate(YearMon = as.yearmon(Date)) %>%
  group_by(Country, YearMon) %>% 
  mutate(MinTemp = mean(Temperature*0.1)) %>%
  select(YearMon, Country, MinTemp) %>%
  distinct(.keep_all = TRUE) 

temp_lt = read_csv("LT_temp.csv") %>%
  mutate(YearMon = as.yearmon(datetime),
         Country = "LT") %>%
  group_by(Country, YearMon) %>%
  mutate(AvgTemp = mean(temp, na.rm = T),
         MaxTemp = mean(tempmax, na.rm = T),
         MinTemp = mean(tempmin, na.rm = T)) %>%
  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
  distinct(.keep_all = TRUE) 

temp_gr = read_csv("GR_temp.csv") %>%
  mutate(YearMon = as.yearmon(datetime),
         Country = "GR")  %>%
  group_by(Country, YearMon) %>%
  mutate(AvgTemp = mean(temp, na.rm = T),
         MaxTemp = mean(tempmax, na.rm = T),
         MinTemp = mean(tempmin, na.rm = T)) %>%
  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
  distinct(.keep_all = TRUE) 

temp_bg = read_csv("BG_temp.csv") %>%
  mutate(YearMon = as.yearmon(datetime),
         Country = "BG") %>%
  group_by(Country, YearMon) %>%
  mutate(AvgTemp = mean(temp, na.rm = T),
         MaxTemp = mean(tempmax, na.rm = T),
         MinTemp = mean(tempmin, na.rm = T)) %>%
  select(YearMon, Country, AvgTemp, MaxTemp, MinTemp) %>%
  distinct(.keep_all = TRUE) 


temp_data = merge(avgtemp,
                  merge(maxtemp, mintemp,  
                        by = c("YearMon", "Country"), all = TRUE),
                  by = c("YearMon", "Country"), all = TRUE )

temp_data_all = rbind(temp_data, temp_gr, temp_lt, temp_bg)

dt = temp_data_all %>% 
  mutate(
    winter = case_when(
      format(YearMon, format = "%m") == "09" ~ 1,
      format(YearMon, format = "%m") == "10" ~ 1,
      format(YearMon, format = "%m") == "11" ~ 1,
      format(YearMon, format = "%m") == "12" ~ 1,
      format(YearMon, format = "%m") == "01" ~ 1,
      format(YearMon, format = "%m") == "02" ~ 1,
      TRUE ~ 0 
    ),
    summer = case_when(
      format(YearMon, format = "%m") == "03" ~ 1,
      format(YearMon, format = "%m") == "04" ~ 1,
      format(YearMon, format = "%m") == "05" ~ 1,
      format(YearMon, format = "%m") == "06" ~ 1,
      format(YearMon, format = "%m") == "07" ~ 1,
      format(YearMon, format = "%m") == "08" ~ 1,
      TRUE ~ 0
    ))

agg_data1 = merge(de, dt, by = c("YearMon", "Country"),  all = TRUE )

#agg_data1  %>%
#  group_by(YearMon, Country) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 1L)

################################################################################
############################## POPULATION DATA #################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_pop")
dp_raw = read_csv("data_pop.csv")

dp = dp_raw %>% 
  filter(TIME_PERIOD == 2021) %>% 
  mutate(Country = geo,
         Population = OBS_VALUE) %>%
  mutate(Country = case_when(
    Country == "EL" ~ "GR",
    TRUE ~ Country
  )) %>%
  select(Country, Population)

agg_data2 = merge(agg_data1, dp, by = "Country", all = TRUE)

#agg_data2  %>%
#  group_by(YearMon, Country) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 1L)

################################################################################
############################# DEGREE DAYS DATA #################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_temp")

hdd = read.csv("hdd.csv") %>%
  mutate(Country = geo,
         YearMon = as.yearmon(TIME_PERIOD),
         HDD = OBS_VALUE,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  filter(Country != "LU") %>%
  select(YearMon, Country, HDD)

cdd = read.csv("cdd.csv") %>%
  mutate(Country = geo,
         YearMon = as.yearmon(TIME_PERIOD),
         CDD = OBS_VALUE,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  filter(Country != "LU") %>%
  select(YearMon, Country, CDD)

ddd = merge(hdd, cdd, by = c("Country", "YearMon"))

agg_data3 = merge(agg_data2, ddd, by = c("Country", "YearMon"), all = TRUE)

#agg_data3  %>%
#  group_by(YearMon, Country) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 1L)

################################################################################
################################### MIDX DATA ##################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_midx")

didx = read_csv("market_idx.csv") %>%
  mutate(Country = geo,
         MarketIDX = OBS_VALUE,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  filter(TIME_PERIOD == 2020) %>%
  mutate(YearMon = as.yearmon("2020-01")) %>%
  select(YearMon, Country, MarketIDX)

agg_data4 = merge(agg_data3, didx, by = c("Country", "YearMon"), all = TRUE)

#agg_data4  %>%
#  group_by(YearMon, Country) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 1L)

################################################################################
################################### CPI DATA ###################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_cpi")

dcpi = read.csv("data_cpi.csv") %>%
  mutate(YearMon = as.yearmon(TIME_PERIOD),
         CPI  = OBS_VALUE,
         Country = geo,
         Country = case_when(
           Country == "EL" ~ "GR",
           TRUE ~ Country
         )) %>%
  select(YearMon, Country, CPI)

agg_data5 = merge(agg_data4, dcpi, by = c("Country", "YearMon"), all = TRUE)

#agg_data5  %>%
#  group_by(YearMon, Country) %>%
#  summarise(n = n(), .groups = "drop") %>%
#  filter(n > 1L)

################################################################################
################################### GDP DATA ###################################
################################################################################

setwd("/Users/Miguel/Desktop/energy_price/data/data_gdp")

dgdp = read_csv("28_gdp.csv") %>% 
  mutate(YearMon = as.yearmon(Date)) %>%
  select(YearMon, Country, GDP)

agg_data_all = merge(agg_data5, dgdp, by = c("Country", "YearMon"), all = TRUE)

setwd("/Users/Miguel/Desktop/energy_price")
write_csv(agg_data_all, "28_monthdata.csv")

### CHECK FOR DUPLICATES 
agg_data_all %>%
  group_by(YearMon, Country) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)




