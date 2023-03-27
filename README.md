## Estimating the effect of the Iberian exception mechanism on different inflation categories for Spain and Portugal

This repository contains replication data and scripts for "Estimating the effect of the Iberian exception mechanism on different inflation categories for Spain and Portugal." It is advised that you take a look at the paper before you try to replicate the results (see *HaroRuiz2023.pdf*). 

The following items are included;

**Files**: 
- *results/* with .csv files generated after running the scripts
- *workspaces/* with .RData files containing all variables generetad through the scripts
- *data/* with .csv files containing montly inflaiton data for 26 countuntries at constant (*26_month_HICP_CT.csv*) and nonconstant taxes (*26_month_HICP.csv*), as well as monthly day-ahead auction prices for 25 countries (*25_month_DAA.csv*)
- *functions/* with a function to compute synthetic controls
 
**Scripts**:
- *trends_inference.R* computes synthetic controls and conducts inference via *iid* and *mb permutations*
- *rmspe_ratios.R* conducts inference via *RMSPE ratios*
- *loo_test.R* performs leave-one-out sensitivity tests

*Note*: The analysis can be performed for CPI at constant depending on whether variable CT is set to TRUE or FALSE in each of the scripts. The *results/* and the *workspaces/* folders contain files with and without the suffix "CT", which corresonds to whether they were produced via CPI data at constant taxes or not. **To replicate the results from the paper, set CT=TRUE**.
