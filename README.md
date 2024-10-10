# CCW_tutorial
This repository includes code to conduct a clone-censor-weighted study of "initiate treatment within X days" using a modified cohort created from the Synthetic Medicare Public Use Files (SynPUF), as well as the base cohort itself. It includes code to study four different regimens: start treatment within 30 days of MI, start treatment within 90 days of MI, start treatment between 30 and 90 days of MI, and never start treatment. The first three regimens are the focus of the associated manuscript.  
  
In addition to the base cohort file built from a modified version of the SynPUF data, this repository includes SAS and R code covering:  
  
-Cloning and censoring the cohort (01_cloning_and_censoring_FINAL)  
  
-Weighting each set of clones to account for selection bias (02a_weighting_0_30_FINAL, 02b_weighting_0_90_FINAL, 02c_weighting_30_90_FINAL, 02d_weighting_no_trt_FINAL)  
  
-Estimating contrasts between each set of clones (03_estimating_effects_final)  
  
-Visualizing the difference in initiation timings across the different regimens (04_visualizing_exposure_FINAL)  
  
-Calculating standardized mean differences to evaluate how well the IPCW balanced covariates at 30 or 90 days, depending on regimen (05a_SMD_assessment_0_30_FINAL, 05b_SMD_assessment_0_90_FINAL, and 05c_SMD_assessment_30_90_FINAL).  
