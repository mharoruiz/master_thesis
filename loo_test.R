################################################################################
#
# This script performs leave-one-out sensitivity tests given a series of 
# preferences. Once the script is run, the results are stored in a data frame 
# called "loo_inference." A .csv file with the results called "loo_test.csv" 
# can found in the results folder. An .RData file named "loo_ws.RData" with 
# the variables after running the scrip can be found in the workspaces folder
#
################################################################################

## Clear environment before every run 
rm(list = ls())

## Import necessary packages
#install.packages("remotes")
#library(remotes) 
#remotes::install_github("kwuthrich/scinference")
library(scinference)
library(tidyverse)
source("functions/sc.R")

set.seed(51231)

## Set preferences, import and clean data

# vector of treated units
treated_units = c("Spain", "Portugal") 
# vector of pre-treatment periods to consider
T0s = c(48, 72, 96) 
# vector of outcome variables
outcomes = c('DAA', 'CP00','GD', 'SERV', 'IGD', 
             'FOOD','NRG', 'IGD_NNRG','TOT_X_NRG')
# logical; whether to use CPI at constant taxes or not
CT = TRUE

# Compute p-values via iid with 1,000, 5,000 and 10,000 permutations and mb permutations 
settings = list( 
  inference = rep("conformal", 4),
  permutation = c("iid",  "iid", "iid", "mb"),
  n_perm = c(1000, 5000, 10000, NULL))

# Import data according to specifications
if (CT == TRUE) {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP_CT.csv") 
} else {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP.csv")
}

# create a dataframe to store the results
loo_inference = data.frame(
  p_val = NA,
  ate = NA,
  placebo_unit = NA,
  outcome = NA,
  n = NA,
  method = NA,
  treated = NA, 
  placebo = NA,
  CT = NA
) 
row = 1

for (tu in treated_units) {
  t_unit = ifelse(tu == "Spain", "ES", "PT" )
  n_unit = ifelse(tu == "Spain", "PT", "ES" )
  print(paste("Treated unit:", tu))
  
  for (n in T0s) {
    print(paste("    Pre-treatment periods:", n)) 
    
    for (out in outcomes) {
      print(paste("        Outcome:", out))
      T0 = ifelse(n == 96 & out == "DAA", 89, n)
      
      ## Filter data according to specifications
      if (out == "DAA" & T0 > 48) {
        d = d.daa |>
          filter(Country != n_unit 
                 & Country != "FR"
                 & Country != "IE"
                 & Country != "BG"
                 & Country != "HR"
          )
      } else if (out == "DAA"){
        d = d.daa |>
          filter(Country != n_unit
                 & Country != "FR"
                 & Country != "IE")
      } else {
        d = d.hicp |>
          filter(Country != n_unit
                 & Country != "FR")
      }
      
      d = d |> filter(YearMon != "2022-06")
      
      countries = unique(d$Country)[-which(unique(d$Country) %in% t_unit)]
      T1 = length(unique(d$timeID[d$post_treat == 1]))
      T01 = T0+T1
  
      Y1 = d |> 
        filter(Country == t_unit) |>
        select(all_of(out)) |>
        slice_tail(n = T01) |>
        as.matrix() |>
        unname()
      
      Y0 = d |> 
        filter(Country != t_unit) |>
        select(YearMon, Country, all_of(out)) |>
        pivot_wider(names_from = Country, values_from = all_of(out)) |>
        select(!YearMon) |>
        slice_tail(n = T01) |>
        as.matrix() |>
        unname()
      
      y1 = Y1[1:(T0), ]
      y0 = Y0[1:(T0), ]
        
      estimate = sc(y1 = y1, y0 = y0, 
                    Y1 = Y1, Y0 = Y0, 
                    lsei_type = 1)
      
      ate = mean(tail(estimate$u.hat, T1)/tail(y1, T1)*100)
      
      idx = which(estimate$w.hat > 0)
      
      for (m1 in 1:length(settings$inference)) {
        
        result = scinference(Y1 = Y1, Y0 = Y0,
                             T1 = T1, T0 = T0,
                             inference_method = settings$inference[m1],
                             alpha = 0.1,
                             ci = FALSE,
                             theta0 = 0,
                             estimation_method = "sc",
                             permutation_method = settings$permutation[m1],
                             n_perm = settings$n_perm[m1],
                             lsei_type = 1,
                             K = 2)
        
        loo_inference[row, ] = c(result$p_val, 
                                     ate, 
                                     NA, 
                                     out, 
                                     n, 
                                     ifelse(settings$permutation[m1] == "iid", paste(settings$permutation[m1], settings$n_perm[m1]), settings$permutation[m1]),
                                     t_unit, 
                                     0,
                                     CT)
        
        row = row +1
        
      } # inference methods
      
      
      for (c in countries[idx]) {
        
        Y0_placebo = d |> 
          filter(Country != t_unit
                 & Country != c) |>
          select(YearMon, Country, all_of(out)) |>
          pivot_wider(names_from = Country, values_from = all_of(out)) |>
          select(!YearMon) |>
          slice_tail(n = T01) |>
          as.matrix() |>
          unname()
        
        
        y0_placebo = Y0_placebo[1:(T0), ]
        
        estimate_placebo = sc(y1 = y1, y0 = y0_placebo,
                              Y1 = Y1, Y0 = Y0_placebo, 
                              lsei_type = 1)
        
        ate_placebo = mean(tail(estimate_placebo$u.hat, T1)/tail(y1, T1)*100)
        
        for (m2 in 1:length(settings$inference)) {
          
          result_placebo = scinference(Y1 = Y1, Y0 = Y0_placebo,
                                       T1 = T1, T0 = T0,
                                       inference_method = settings$inference[m2],
                                       alpha = 0.1,
                                       ci = FALSE,
                                       theta0 = 0,
                                       estimation_method = "sc",
                                       permutation_method = settings$permutation[m2],
                                       n_perm = settings$n_perm[m2],
                                       lsei_type = 1,
                                       K = 2)
          
          loo_inference[row, ] = c(result_placebo$p_val, 
                                       ate_placebo, 
                                       c, 
                                       out, 
                                       n, 
                                       ifelse(settings$permutation[m2] == "iid", paste(settings$permutation[m2], settings$n_perm[m2]), settings$permutation[m2]),
                                       t_unit, 
                                       1, 
                                       CT)
          
          row = row +1
          
          } # inference methods
        } # countries
      } # outcomes
    } #T0s
  } # treated_units

if (CT == TRUE) {
  write_csv(loo_inference, "results/loo_test_CT.csv")
} else {
  write_csv(loo_inference, "results/loo_test.csv")
}

