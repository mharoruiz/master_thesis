## master_thesis

This repository contains the replication for "Estimating the effect of the Iberian exception mechanism on different CPI categories for Spain and Portugal." It is advice that you take a look at the paper (see HaroRuiz.pdf) before you try to replicate the resutls. 

The following items are included;

**Files**: 
- *results/* with .csv files created after running the scripts
- *workspaces/* wiht .RData files with all resulting variables after running each script
- *data/* with .csv files to perform the analysis
- *functions/* wiht a function to compute synthetic controls
 
**Scripts**:
- *trends_inference.R* computes synthetic controls and inference via *iid* and *mb permutations*
- *rmspe_ratios.R* computes inference via *RMSPE ratios*
- *loo_test.R* performs leave-one-out sensitivity tests for the results

*Note*: The analysis can be performed for CPI at constant taxes (via 26_month_HICP_CT.csv) or variable taxes (via 26_month_HICP.csv) depending on whether variable CT is set to TRUE or FALSE in each of the scripts. The results/ and the workspaces/ folders contain files with and without the suffix "CT", which corresonds to whether they were produced with CPI at constant taxes or not. **To replicate the results from the paper, set CT=TRUE.**
      
