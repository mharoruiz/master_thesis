rm(list = ls())

library(tidyverse)
library(tidysynth)
library(zoo)

setwd("/Users/Miguel/Desktop/master_thesis")
d.raw = read_csv("25_yearmon.csv") %>% 
  filter(YearMon >= as.yearmon("Jun 2019")
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
#%>%
  select(Country, monthID,
         net_imports, nuclear_load, nrenew_load, 
         renew_load, infra_load, DAPrice,
         GDPcap, Loadcap,
         CP00, CP01, CP02, CP03, CP04, CP05, CP06, 
         CP07, CP08, CP09, CP10, CP11, CP12) %>%
  mutate_at(c("net_imports", "nuclear_load", "nrenew_load", 
              "renew_load", "infra_load", "DAPrice",
              "GDPcap", "Loadcap",
              "CP00", "CP01", "CP02", "CP03", "CP04", "CP05", "CP06", 
              "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"), ~(scale(.) %>% as.vector))

d_ref = d %>%
  select(Country, YearMon, monthID, treat, mean_HDD, mean_CDD)


################################################################################
###CP00: All-items 
################################################################################

res00 = d %>%
  
  synthetic_control(outcome = CP00, 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%

  
  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res00 %>% plot_weights()
res00 %>% grab_balance_table()
res00 %>% plot_mspe_ratio()
print(res00 %>% grab_signficance, n = 25)

res00 %>% plot_trends()
res00 %>% plot_differences()

res00 %>% plot_placebos()
res00 %>% plot_placebos(prune = FALSE)

################################################################################
### CP01: Food and non-alcoholic beverages
################################################################################

res01 = d %>%
  
  synthetic_control(outcome = CP01, # 0.289
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res01 %>% plot_weights()
res01 %>% grab_balance_table()
res01 %>% plot_mspe_ratio()
print(res01 %>% grab_signficance, n = 25)

res01 %>% plot_trends()
res01 %>% plot_differences()

res01 %>% plot_placebos()
res01 %>% plot_placebos(prune = FALSE)

################################################################################
### CP02: Alcoholic beverages and tobacco
################################################################################

res02 = d %>%
  
  synthetic_control(outcome = CP02, # -0.805 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res02 %>% plot_weights()
res02 %>% grab_balance_table()
res02 %>% plot_mspe_ratio()
print(res02 %>% grab_signficance, n = 25)

res02 %>% plot_trends()
res02 %>% plot_differences()

res02 %>% plot_placebos()
res02 %>% plot_placebos(prune = FALSE)

################################################################################
### CP03: Clothing and footwear
################################################################################

res03 = d %>%
  
  synthetic_control(outcome = CP03, # -1.00 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res03 %>% plot_weights()
res03 %>% grab_balance_table()
res03 %>% plot_mspe_ratio()
print(res03 %>% grab_signficance, n = 25)

res03 %>% plot_trends()
res03 %>% plot_differences()

res03 %>% plot_placebos()
res03 %>% plot_placebos(prune = FALSE)

################################################################################
### CP04: Housing, water, electricity, gas and other fuels
################################################################################

res04 = d %>%
  
  synthetic_control(outcome = CP04, # -0.796 
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 7,
  #                   HDD_2021 = sum_HDD,
  #                   CDD_2021 = sum_CDD
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res04 %>% plot_weights()
res04 %>% grab_balance_table()
res04 %>% plot_mspe_ratio()
print(res04 %>% grab_signficance, n = 25)

res04 %>% plot_trends()
res04 %>% plot_differences()

res04 %>% plot_placebos()
res04 %>% plot_placebos(prune = FALSE)

################################################################################
### CP05: Furnishings, Household equipment and routine maintenance of the house
################################################################################

res05 = d %>%
  
  synthetic_control(outcome = CP05,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res05 %>% plot_weights()
res05 %>% grab_balance_table()
res05 %>% plot_mspe_ratio()
print(res05 %>% grab_signficance, n = 25)

res05 %>% plot_trends()
res05 %>% plot_differences()

res05 %>% plot_placebos()
res05 %>% plot_placebos(prune = FALSE)

################################################################################
### CP06: Health
################################################################################

res06 = d %>%
  
  synthetic_control(outcome = CP06,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res06 %>% plot_weights()
res06 %>% grab_balance_table()
res06 %>% plot_mspe_ratio()
print(res06 %>% grab_signficance, n = 25)

res06 %>% plot_trends()
res06 %>% plot_differences()

res06 %>% plot_placebos()
res06 %>% plot_placebos(prune = FALSE)

################################################################################
### CP07: Transport
################################################################################

res07 = d %>%
  
  synthetic_control(outcome = CP07,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
   ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res07 %>% plot_weights()
res07 %>% grab_balance_table()
res07 %>% plot_mspe_ratio()
print(res07 %>% grab_signficance, n = 25)

res07 %>% plot_trends()
res07 %>% plot_differences()

res07 %>% plot_placebos()
res07 %>% plot_placebos(prune = FALSE)

################################################################################
### CP08: Communication
################################################################################

res08 = d %>%
  
  synthetic_control(outcome = CP08,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res08 %>% plot_weights()
res08 %>% grab_balance_table()
res08 %>% plot_mspe_ratio()
print(res08 %>% grab_signficance, n = 25)

res08 %>% plot_trends()
res08 %>% plot_differences()

res08 %>% plot_placebos()
res08 %>% plot_placebos(prune = FALSE)

################################################################################
### CP09: Recreation and Culture
################################################################################

res09 = d %>%
  
  synthetic_control(outcome = CP09,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res09 %>% plot_weights()
res09 %>% grab_balance_table()
res09 %>% plot_mspe_ratio()
print(res09 %>% grab_signficance, n = 25)

res09 %>% plot_trends()
res09 %>% plot_differences()

res09 %>% plot_placebos()
res09 %>% plot_placebos(prune = FALSE)

################################################################################
### CP10: Education
################################################################################

res10 = d %>%
  
  synthetic_control(outcome = CP10,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res10 %>% plot_weights()
res10 %>% grab_balance_table()
res10 %>% plot_mspe_ratio()
print(res10 %>% grab_signficance, n = 25)

res10 %>% plot_trends()
res10 %>% plot_differences()

res10 %>% plot_placebos()
res10 %>% plot_placebos(prune = FALSE)

################################################################################
### CP11: Restaurants and hotels
################################################################################

res11 = d %>%
  
  synthetic_control(outcome = CP11,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res11 %>% plot_weights()
res11 %>% grab_balance_table()
res11 %>% plot_mspe_ratio()
print(res11 %>% grab_signficance, n = 25)

res11 %>% plot_trends()
res11 %>% plot_differences()

res11 %>% plot_placebos()
res11 %>% plot_placebos(prune = FALSE)

################################################################################
### CP12: Miscellaneous goods and services
################################################################################

res12 = d %>%
  
  synthetic_control(outcome = CP12,
                    unit    = Country,
                    time    = monthID, 
                    i_unit  = "ES", 
                    i_time  = 37, 
                    generate_placebos = T
  ) %>%
  
  generate_predictor(time_window = 13:36,
                     
                     Import_Share    = mean(net_imports, na.rm = T),
                     Nuclear_Share   = mean(nuclear_load, na.rm = T),
                     NonRenew_Share  = mean(nrenew_load, na.rm = T),
                     Renew_Share     = mean(renew_load, na.rm = T),
                     Infra_Share     = mean(infra_load, na.rm = T),
                     
                     Load_perCapita  = mean(Loadcap, na.rm = T),
                     GDP_perCapita   = mean(GDPcap, na.rm = T),
                     
                     #Day_Ahead   = mean(DAPrice, na.rm = T), 
                     
                     #NetImports     = mean(NetIM, na.rm = T),
                     #Nuclear        = mean(Nuclear, na.rm = T),
                     #NonRenewable   = mean(NRenew, na.rm = T),
                     #Renewable      = mean(Renew, na.rm = T),
                     
                     #Load           = mean(Load, na.rm = T),
                     #GDP            = mean(GDP, na.rm = T),
  ) %>%
  
  generate_predictor(time_window = 25:36,
                     Day_Ahead   = mean(DAPrice, na.rm = T)
  ) %>%
  
  #generate_predictor(time_window = 13:24,
  #                   HDD_2021 = sum(HDD, na.rm = TRUE),
  #                   CDD_2021 = sum(CDD, na.rm = TRUE)
  #) %>%
  
  #generate_predictor(time_window = 1,
  #                   Market_2020 = Generation,
  #                   
  #) %>%
  

  generate_weights(optimization_window = 1:36,
                   #optimization_method = "All",
                   margin_ipop = .02,
                   sigf_ipop   = 7,
                   bound_ipop  = 6 
  ) %>%
  
  generate_control()

res12 %>% plot_weights()
res12 %>% grab_balance_table()
res12 %>% plot_mspe_ratio()
print(res12 %>% grab_signficance, n = 25)

res12 %>% plot_trends()
res12 %>% plot_differences()

res12 %>% plot_placebos()
res12 %>% plot_placebos(prune = FALSE)
