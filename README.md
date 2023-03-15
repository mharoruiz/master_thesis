## master_thesis

This repository contains the replication files for my master's thesis. 

The following folders are included:
- results/ includes .csv files created after running the scripts
- workspaces/ contains .RData files with all resulting variables after running each script
- data/ includes .csv files to perform the analysis
- functions/ contains a function to compute synthetic controls 

The following scripts are included:
- trends_inference.R computes synthetic controls and inference via *iid* and *mb permutations*
- rmspe_ratios.R computes inference via *RMSPE ratios*
- loo_test.R performs leave-one-out sensitivity tests for the results

*Note*: The analysis can be performed for CPI at constant taxes (via 26_month_HICP_CT.csv) or variable taxes (via 26_month_HICP.csv) depending on whether variable CT is set to TRUE or FALSE in each of the scripts. The results/ and the workspaces/ folders contain files with and without the suffix "CT", which corresonds to whether they were produced with CPI at constant taxes or not. **To replicate the results from the paper, set CT=TRUE.**
      
