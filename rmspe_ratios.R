################################################################################
#
# This script computes p-values via RMSPE ratios  given a series of 
# preferences.  Once the script is run, the results are stored in a data frame 
# called "agg_rmspe". A .csv file with the results called "agg_rmspe.csv" 
# can found in the results folder. An .RData file named "rmspe_ws.RData" 
# with the variables after running the scrip can be found in the workspaces folder
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

set.seed(68381)

rmse = function(x){sqrt(mean(x^2))}

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

# Import data according to specifications
if (CT == TRUE) {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP_CT.csv") 
} else {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP.csv")
}

agg_rmspe = NULL 
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
      if (out == "DAA" & n > 48) {
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
      
      countries = unique(d$Country)[!n_unit %in% unique(d$Country)]
      T1 = length(unique(d$timeID[d$post_treat == 1]))
      T01 = T0+T1
      
      ratios = data.frame(
        unit = NA,
        rmspe_ratio = NA)
      r = 1
      for (c in countries) {
        
        Y1 = d |> 
          filter(Country == c) |>
          select(all_of(out)) |>
          slice_tail(n = T01 )|>
          as.matrix() |>
          unname()
        
        Y0 = d |> 
          filter(Country != c) |>
          select(YearMon, Country, all_of(out)) |>
          pivot_wider(names_from = Country, values_from = all_of(out)) |>
          select(!YearMon) |>
          slice_tail(n = T01 ) |>
          as.matrix() |>
          unname()
        
        y1 = Y1[1:(T0), ]
        y0 = Y0[1:(T0), ]
        
        estimate = sc(y1 = y1, y0 = y0, 
                      Y1 = Y1, Y0 = Y0, 
                      lsei_type = 1)
        gaps = Y1 - estimate$Y0.hat
        
        pre_rmspe = rmse(gaps[1:T0,])
        post_rmspe = rmse(gaps[(T0+2):T01, ])
        ratio = post_rmspe/pre_rmspe
        
        ratios[r, ] = c(c, ratio)
        r = r +1
        } # country
      
      rmspe = 
        ratios |>
        mutate(rank = dense_rank(desc(rmspe_ratio)),
               p_val = rank/length(countries),
               n = n, 
               outcome = out, treated = t_unit, 
               CT = CT)
      
      agg_rmspe = rbind(agg_rmspe, rmspe)
        
      
      } # outcome
    } # T0
} # treated

if (CT == TRUE) {
  write_csv(agg_rmspe, "results/rmspe_CT.csv")
} else {
  write_csv(agg_rmspe, "results/rmspe.csv")
}


