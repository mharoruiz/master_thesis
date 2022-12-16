rm(list = ls())

library(tidyverse)
library(tidysynth)
library(zoo)

setwd("/Users/Miguel/Desktop/master_thesis")
d.raw = read_csv("25_yearmon.csv") %>% 
  filter(YearMon >= as.yearmon("Jun 2021")
         & YearMon <= as.yearmon("Oct 2022")
  )

d = d.raw  %>%
  filter(Country != "PT"
         #& Country != "RS" # HDD/CHH 
         #& Country != "NL" # MarketStructure
         #& Country != "NO" # MarketStructure
         #& Country != "AT" # MarketStructure
  ) %>%
  #filter(Region != "NE") %>% 
  mutate(monthID = unclass(factor(YearMon, 
                                  unique(d.raw$YearMon))),
         net_imports  = NetIM/Load,
         nuclear_load = Nuclear/Load,
         nrenew_load  = NRenew/Load,
         renew_load   = Renew/Load,
         infra_load   = (Renew + Nuclear)/Load,
         GDPcap  = (GDP/Population)*1000000,
         Loadcap = Load/Population
  )

d_ref = d %>%
  select(Country, YearMon, monthID, treat, mean_HDD, mean_CDD)

################################################################################
### CP045: Electricity, gas and other fuels
################################################################################

res045 = d %>%
  
  synthetic_control(outcome = CP045, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res045 %>% plot_weights()
res045 %>% grab_balance_table()
res045 %>% plot_mspe_ratio()
print(res045 %>% grab_signficance, n = 25)

res045 %>% plot_trends()
res045 %>% plot_differences()

res045 %>% plot_placebos()
res045 %>% plot_placebos(prune = FALSE)

################################################################################
### CP0451: Electricity
################################################################################

res0451 = d %>%
  
  synthetic_control(outcome = CP0451, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res0451 %>% plot_weights()
res0451 %>% grab_balance_table()
res0451 %>% plot_mspe_ratio()
print(res0451 %>% grab_signficance, n = 25)

res0451 %>% plot_trends()
res0451 %>% plot_differences()

res0451 %>% plot_placebos()
res0451 %>% plot_placebos(prune = FALSE)

################################################################################
### CP0452: Gas
################################################################################

res0452 = d %>%
  
  synthetic_control(outcome = CP0452, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res0452 %>% plot_weights()
res0452 %>% grab_balance_table()
res0452 %>% plot_mspe_ratio()
print(res0452 %>% grab_signficance, n = 25)

res0452 %>% plot_trends()
res0452 %>% plot_differences()

res0452 %>% plot_placebos()
res0452 %>% plot_placebos(prune = FALSE)

################################################################################
### CP0453: Liquid fuels
################################################################################

res0453 = d %>%
  
  synthetic_control(outcome = CP0453, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res0453 %>% plot_weights()
res0453 %>% grab_balance_table()
res0453 %>% plot_mspe_ratio()
print(res0453 %>% grab_signficance, n = 25)

res0453 %>% plot_trends()
res0453 %>% plot_differences()

res0453 %>% plot_placebos()
res0453 %>% plot_placebos(prune = FALSE)

################################################################################
### CP0454: Solid fuels
################################################################################

res0454 = d %>%
  
  synthetic_control(outcome = CP0454, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res0454 %>% plot_weights()
res0454 %>% grab_balance_table()
res0454 %>% plot_mspe_ratio()
print(res0454 %>% grab_signficance, n = 25)

res0454 %>% plot_trends()
res0454 %>% plot_differences()

res0454 %>% plot_placebos()
res0454 %>% plot_placebos(prune = FALSE)

################################################################################
### CP0455: Heat energy
################################################################################

res0455 = d %>%
  
  synthetic_control(outcome = CP0455, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  
  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()


res0455 %>% plot_weights()
res0455 %>% grab_balance_table()
res0455 %>% plot_mspe_ratio()
print(res0455 %>% grab_signficance, n = 25)

res0455 %>% plot_trends()
res0455 %>% plot_differences()

res0455 %>% plot_placebos()
res0455 %>% plot_placebos(prune = FALSE)

################################################################################
### NRG: Energy
################################################################################

resNRG = d %>%
  
  synthetic_control(outcome = NRG, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  
  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()


resNRG %>% plot_weights()
resNRG %>% grab_balance_table()
resNRG %>% plot_mspe_ratio()
print(resNRG %>% grab_signficance, n = 25)

resNRG %>% plot_trends()
resNRG %>% plot_differences()

resNRG %>% plot_placebos()
resNRG %>% plot_placebos(prune = FALSE)

################################################################################
### ELC_GAS: Electricity, gas, solid fuels and heat energy
################################################################################

resELC_GAS = d %>%
  
  synthetic_control(outcome = ELC_GAS, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 13, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 1:12,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T), 
                     Day_Ahead       = mean(DAPrice, na.rm = T),
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  
  generate_weights(optimization_window = 1:12,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()


resELC_GAS %>% plot_weights()
resELC_GAS %>% grab_balance_table()
resELC_GAS %>% plot_mspe_ratio()
print(resELC_GAS %>% grab_signficance, n = 25)

resELC_GAS %>% plot_trends()
resELC_GAS %>% plot_differences()

resELC_GAS %>% plot_placebos()
resELC_GAS %>% plot_placebos(prune = FALSE)

