## master_thesis

This repository contains the replication files for my master's thesis. 

The following folders are included:
- data/ contains .csv files to perform the analysis
- functions/ includes a function to compute synthetic controls 
- workspaces/ contains .RData files with all resulting variables after running each scrip

The following scripts are included:
- trends_inference.R computes synthetic controls and inference via iid and mb permutations
- rmspe_ratios.R computes inference via RMSPE ratios
- loo_test.R performs leave-one-out sensitivity tests for the results

*Note*: The analysis can be performed for CPI at constant taxes (via 26_month_HICP_CT.csv) or varying taxes (via 26_month_HICP.csv) depending on whether variable CT=TRUE or CT=FALSE, respectively in each of the scripts. The results and the workspaces folders contain files with and without the suffix "CT", which corresond to whether they were produced for CPI at constant taxes or not. **To replicate the results from the paper, set CT=TRUE.**
      
