################################################################################
#
# This script computes either observed and synthetic trends with confidence 
# intervals OR p-values, given a series of outcomes and specifications.
#
# To compute trends, set compute_trends = TRUE (this takes a considerable 
# amount of computational time). The output is stored in a data frame named
# "agg_trends." A .csv with the output named "trends.csv" can be found
# in the results folder. An .RData file named "trends_ws.RData" with 
# the variables after running the scrip can be found in the workspaces folder
# 
# To compute p-values, set compute_inference = TRUE. The output is stored in a 
# data frame named  "agg_inference." A .csv with the output named 
# "inference.csv" can be found in the results folder. An .RData file named 
# "inference_ws.RData" with  the variables after running the scrip can be 
# found in the workspaces folder
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

compute_trends = FALSE
compute_inference = TRUE

if (compute_trends == TRUE) {
  settings = list( 
    inference = c("conformal"),
    ci = TRUE,
    permutation = "iid",
    n_perm = 5000)
} else if (compute_inference == TRUE) {
  settings = list( 
    inference = rep("conformal", 4),
    ci = rep(FALSE, 4),
    permutation = c("iid",  "iid", "iid", "mb"),
    n_perm = c(1000, 5000, 10000, NULL))
}

# Import data according to specifications
if (CT == TRUE) {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP_CT.csv") 
} else {
  d.daa = read_csv("data/25_month_DAA.csv") 
  d.hicp = read_csv("data/26_month_HICP.csv")
}

agg_trends = NULL 
agg_inference = NULL
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
      
      if (compute_inference == TRUE) {
        d = d |> filter(YearMon != "2022-06")
      }

      T1 = length(unique(d$timeID[d$post_treat == 1]))
      T01 = T0+T1
      
      time_range = d |>
        slice_tail(n = T01) |> 
        mutate(date = paste(YearMon, "15", sep = "-")) |>
        select(date) |>
        as.matrix() |>
        unname() 
      
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
      
      if (compute_trends == TRUE) {
        
        y1 = Y1[1:T0, ]
        y0 = Y0[1:T0, ]
        
        estimate = sc(y1 = y1, y0 = y0, 
                      Y1 = Y1, Y0 = Y0, 
                      lsei_type = 1)
        gaps = estimate$u.hat
        
        if (out == "DAA") {
          grid = seq(-205, 8, by = 0.001)
        } else {
          grid = seq(round((max(abs(gaps))*-1.25), 3), 
                     round((max(gaps)+(max(abs(gaps))*.33)), 3),
                     by = 0.001)
          }
        }
      
      for (m in 1:length(settings$inference)) {
        
        if (settings$inference[m] == "conformal") {
          print(paste("            Method:", 
                      settings$inference[m], 
                      settings$permutation[m]) )
        } else if (settings$inference[m] == "ttest") {
          print(paste("            Method:", 
                      settings$inference[m] )) 
          }
    
        confidence = FALSE
        reps = 1
        while(confidence == FALSE) {
          result = scinference(Y1 = Y1, Y0 = Y0,
                               T1 = T1, T0 = T0,
                               inference_method = settings$inference[m],
                               alpha = 0.1,
                               ci = settings$ci[m],
                               theta0 = 0,
                               estimation_method = "sc",
                               permutation_method = settings$permutation[m],
                               ci_grid = grid,
                               n_perm = settings$n_perm[m],
                               lsei_type = 1,
                               K = 2)
          if (compute_trends == TRUE) {
            
            if (min(grid) %in% result$lb & max(grid) %in% result$ub) {
              grid = seq(round(min(grid)*1.33, 3), 
                         round(max(grid)*1.33, 3), 
                         by = 0.001)
              print("                Both limits updated")
            } else if (max(grid) %in% result$ub) {
              grid = seq(min(grid), 
                         round(max(grid)*1.5, 3), 
                         by = 0.001)
              print("                Upper limit updated")
            } else if  (min(grid) %in% result$lb) {
              grid = seq(round(min(grid)*1.5, 3), 
                         max(grid), 
                         by = 0.001)
              print("                Lower limit updated")
            } else {
              confidence = TRUE
            }
          } else {
            confidence = TRUE
          }
          }
        
        if (settings$inference[m] == "conformal") {
          if (compute_trends == TRUE) {
            trends = data.frame(
              date = time_range, 
              obs = Y1, 
              synth = estimate$Y0.hat,
              gaps = gaps,
              upper = c(rep(NA, T0), result$ub),
              lower = c(rep(NA, T0), result$lb),
              method = settings$permutation[m], 
              n = n, 
              outcome = out, 
              treated = t_unit,
              CT = CT
            )
          }
          if (compute_inference == TRUE) {
            inference = data.frame(
              p_val = result$p_val,
              att = NA, 
              se = NA,
              method = ifelse(settings$permutation[m] == "iid", paste(settings$permutation[m], settings$n_perm[m]), settings$permutation[m]),
              n = n, 
              outcome = out, 
              treated = t_unit,
              CT = CT
            )
          }
          
        } else if (settings$inference[m] == "ttest") {
          
          upper_bound = result$ub - result$att
          lower_bound = result$att - result$lb
          
          if (compute_trends == TRUE) {
            trends = data.frame(
              date = time_range, 
              obs = Y1, 
              synth = estimate$Y0.hat,
              gaps = gaps,
              upper = c(rep(NA, T0), gaps[(T0+1):T01] + upper_bound),
              lower = c(rep(NA, T0), gaps[(T0+1):T01] - lower_bound),
              method = settings$inference[m],
              n = n, 
              outcome = out, 
              treated = t_unit,
              CT = CT
            )
          }
          if (compute_inference == TRUE) {
            inference = data.frame(
              p_val = NA,
              att = result$att,
              se =  result$se,
              method = settings$inference[m],
              n = n, 
              outcome = out, 
              treated = t_unit,
              CT = CT
            )
          }
        }
        if (compute_trends == TRUE) {agg_trends = rbind(agg_trends, trends)}
        if (compute_inference == TRUE) {agg_inference = rbind(agg_inference, inference)}
      }
    }
  }
}

if (CT == TRUE) {
  if (compute_trends == TRUE) {write_csv(agg_trends, "results/trends_CT.csv")}
  if (compute_inference == TRUE) {write_csv(agg_inference, "results/inference_CT.csv")}
} else {
  if (compute_trends == TRUE) {write_csv(agg_trends, "results/trends.csv")}
  if (compute_inference == TRUE) {write_csv(agg_inference, "results/inference.csv")} 
}



