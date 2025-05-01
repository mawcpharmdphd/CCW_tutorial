
getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(broom)

notrt_with_cens <- read.csv("~/Desktop/Michael CCW project/R/notrt_with_cens.csv")


# CENSORING WEIGHTS FOR THE "UNTREATED" ARM
  
# This set has two forms of censoring: general loss to follow-up and censoring because of starting a prescription. While we can assume the former 
# is basically random, we should for censoring due to the latter by fitting inverse probability of remaining uncensored weights. We also
# need to make sure we account for the "t0" censoring that occurs. Let's take care of the time 0 censoring first in its own step.

# First, we fit a model in the notrt data set predicting the probability of being uncensored at time 0 (i.e., t0censnotrt = 0) based on baseline 
# covariates including renal, baseline age, and sex. We'll use GENMOD for multivariable logistic regression. Because we want to predict
# the probability of being UNCENSORED, we should not use a DESCENDING statement.


model_notrt_t0_uncens <- glm(
  t0cens30_90 ~ baseline_age + sex + renal, 
  family = binomial(link = "logit"), 
  data = notrt_with_cens)

notrt_t0_uncens <- notrt_with_cens %>%
  mutate(t0_uncens = 1 - predict(model_notrt_t0_uncens, type = "response"))


notrt_with_cens_no_0_fu <- notrt_t0_uncens %>%
  filter(t0censnotrt != 1) %>%  
  mutate(t0IPCW = 1 / t0_uncens) 

nrow(notrt_with_cens_no_0_fu)
# 1585





long_notrt <- notrt_with_cens_no_0_fu %>%
  # Transform Discharge_date to a proper Date object that allows arithmetic with days
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>%
  
  # Generate two rows per individual with start_interval = 0 and 90
  crossing(start_interval = seq(from = 0, to = 170, by = 10)) %>%
  
  # Correspond to "IF Cens_followup <= (start_interval + 10) THEN LEAVE" in SAS, which menas that
  # when start_interval >= Cens_followup - 10, still generate obs, 
  # but from the next interval, i.e. start_interval >= Cens_followup, don't generate obs
  filter(!(start_interval >= Cens_followup)) %>% 
  
  mutate(date = discharge_date + start_interval) %>%
  
  unnest_longer(start_interval) %>%
  
  mutate(date = discharge_date + start_interval,
    intv_age = as.integer(difftime(date, birthdate, units = "days") / 365.25),  
    
    long_outcome = case_when(
      Cens_followup > (start_interval + 10) ~ 0,
      Cens_followup <= (start_interval + 10) ~ Cens_outcome
      ),
    
    long_cens_start = case_when(
      Cens_followup > (start_interval + 10) ~ 0,
      Cens_followup <= (start_interval + 10) ~ Cens_start
      ),
    
    interval_fu = case_when(
      Cens_followup > (start_interval + 10) ~ 10,
      Cens_followup <= (start_interval + 10) ~ Cens_followup - start_interval
      ),
    
    end_interval = case_when(
      Cens_followup > (start_interval + 10) ~ start_interval + 10,
      Cens_followup <= (start_interval + 10) ~ Cens_followup
      )
    
  ) %>% ungroup()


# /*Now, we can fit a pooled multivariable logistic model predicting the probability of long_cens_start being equal to 0 (i.e., remaining uncensored)
# across the entire follow-up period. We probably want to include a term for start_interval with both linear and quadratic terms to capture time trends somewhat flexibly.*/
  
long_notrt_model <- glm(
  long_cens_start ~ intv_age + sex + renal + start_interval + I(start_interval^2),
  family = binomial(link = "logit"),
  data = long_notrt
  )

long_notrt_for_IPCW <- long_notrt %>%
  mutate(FU_uncens = 1 - predict(long_notrt_model, type = "response"))



long_no_trt_weights <- long_notrt_for_IPCW %>%
  group_by(ID) %>%
  mutate(
    Interval_IPCW = 1 / FU_uncens) %>% 
  mutate(
    Cumulative_IPCW = {
      cum_ipcw <- t0IPCW[1] * Interval_IPCW[1]
      out <- numeric(n())  # Preallocate space
      out[1] <- cum_ipcw
      
      if (n() > 1) {
        for (i in 2:n()) {
          out[i] <- Interval_IPCW[i] * out[i - 1]
        }
      }
      out
    }
  ) %>%
  ungroup()


summary(long_no_trt_weights$Interval_IPCW)
summary(long_no_trt_weights$Cumulative_IPCW)
