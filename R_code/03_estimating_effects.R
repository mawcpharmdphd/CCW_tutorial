
getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(survival)

# ESTIMATING EFFECTS
   
# We now have four weighted data sets representing potential mortality (our outcome) from day 0 to 180 under four distinct hypothetical interventions following myocardial infarction:
# 1) No treatment initiation 
# 2) Treatment initiation within day 0 to 90
# 3) Treatment initiation within day 0 to 30
# 4) Treatment initiation from day 30 to 90

# We can contrast the mortality within each of these data sets using our effect measure of interest, the risk difference.
# Suppose we are interested in comparing the risk difference for 180 day mortality comparing the "0 to 90" intervention with the "0-30" intervention as a referent.


### First, we should load in our referent group, "0-30 day." ###
treatment_30 <- wted_trt_0_30

cox_model_trt30 <- coxph(
  Surv(start_interval, end_interval, long_outcome) ~ 1,  
  data = treatment_30,
  weight = Cumulative_IPCW,  
  ties = "efron"  
  )

summary(cox_model_trt30)

# Generate the baseline survival curve
treatment_30_surv_curv <- survfit(cox_model_trt30)

# Extract the survival probabilities
surv_trt_30 <- broom::tidy(treatment_30_surv_curv) %>%
  select(time, estimate) 

treatment_30_180_day <- surv_trt_30 %>%
  mutate(
    risk_trt_30 = 1 - estimate  
  ) %>%
  slice_tail(n = 1)  


### We can do the same thing to the treatment initiation within day 0 to 90 group ###
treatment_90 <- wted_trt_0_90

cox_model_trt90 <- coxph(
  Surv(start_interval, end_interval, long_outcome) ~ 1,  
  data = treatment_90,
  weight = Cumulative_IPCW,  
  ties = "efron"  
)

summary(cox_model_trt90)

treatment_90_surv_curv <- survfit(cox_model_trt90)

surv_trt_90 <- broom::tidy(treatment_90_surv_curv) %>%
  select(time, estimate) 


treatment_90_180_day <- surv_trt_90 %>%
  mutate(
    risk_trt_90 = 1 - estimate 
  ) %>%
  slice_tail(n = 1)  
treatment_90_180_day


### We can do the same thing to the treatment initiation within day 30 to 90 group ###
treatment_30_90 <- wted_trt_30_90

cox_model_trt30_90 <- coxph(
  Surv(start_interval, end_interval, long_outcome) ~ 1,  
  data = treatment_30_90,
  weight = Cumulative_IPCW,  
  ties = "efron"  
)

summary(cox_model_trt30_90)

treatment_30_90_surv_curv <- survfit(cox_model_trt30_90)

surv_trt_30_90 <- broom::tidy(treatment_30_90_surv_curv) %>%
  select(time, estimate) 

treatment_30_90_180_day <- surv_trt_30_90 %>%
  mutate(
    risk_trt_30_90 = 1 - estimate 
  ) %>%
  slice_tail(n = 1)  


treatment_90_180_day$risk_trt_90
treatment_30_180_day$risk_trt_30
treatment_30_90_180_day$risk_trt_30_90

# Risk difference
treatment_90_180_day$risk_trt_90 - treatment_30_180_day$risk_trt_30
treatment_30_90_180_day$risk_trt_30_90 - treatment_30_180_day$risk_trt_30

