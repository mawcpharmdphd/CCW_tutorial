
getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(lubridate)

trt_30_90_with_cens <- read.csv("~/Desktop/Michael CCW project/R/trt_30_90_with_cens.csv")

### CENSORING WEIGHTS FOR THE 30-90 DAY WINDOW
  
# Now, let's weight the trt_30_90 data set. This incorporates some elements of the 
# "trt_0_30" and "trt_0_90" datasets with additional censoring during the first 30 days.

# This set has three forms of censoring: 
# 1) general loss to follow-up
# 2) censoring because patients starting taking medications from day 0 to 30 (Cens_startearly = 1)
# 3) censoring because patients did not start taking medications by day 90 (cens_nostart = 1)

# We will be assuming that general loss to follow-up is random, meaning we only 
# have potential selection bias from "Cens_startearly = 1" before day 30 and "cens_nostart=1" at day 90.

# Before doing anything, we need to deal with the day 0 censoring.
model_t0_uncens <- glm(
  t0cens30_90 ~ baseline_age + sex + renal, 
  family = binomial(link = "logit"), 
  data = trt_30_90_with_cens)


trt_30_90_t0_uncens <- trt_30_90_with_cens %>%
  mutate(t0_uncens = 1 - predict(model_t0_uncens, type = "response"))


# Now we can generate a new data set with those who were not censored at t0, assigning them 
# weights based on these conditional probabilities to counter the t0 censoring. 
# We can call those weights "t0IPCW" to represent their action at t0.
  
trt_30_90_with_cens_no_0_fu <- trt_30_90_t0_uncens %>%
  filter(t0cens30_90 != 1) %>% 
  mutate(t0IPCW = 1 / t0_uncens)


# Next, let's make the dataset long. Unlike the trt_0_30 and trt_0_90 analyses, 
# we will need more than two observations. This is because we will need to account
# for censoring from Cens_startearly = 1 during the 0-30 day window. 
# We can do this with 30 one-day intervals starting on day 0. 
# We will then have an interval from day 30 to 90 where no one is censored for either 
# starting early or not starting, and then a final interval from day 90 to 180 
# where we deal with the censoring from cens_nostart = 1.



### start_interval = c(0:29) ### 
long_trt_30_90_first30days <- trt_30_90_with_cens_no_0_fu %>%
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>%
  
  # Generate observations covering the first 30 days, 1 day/row
  crossing(start_interval = c(0:29)) %>% 
  
  # Calculate the new date for each interval
  mutate(date = discharge_date + start_interval) %>%
  
  # generate multiple rows for each individual based on intervals
  unnest_longer(start_interval) %>%
  
  # don't generate rows when start_interval begins to be > Cens_followup
  filter(!start_interval > Cens_followup) %>% 
  
  mutate(date = discharge_date + start_interval,  
    intv_age = as.integer(difftime(date, birthdate, units = "days") / 365.25),  
    
    long_cens_nostart = 0, # No one in this time interval can be censored for not starting treatment 

    long_outcome = case_when(
      Cens_followup > (start_interval + 1) ~ 0, # They did not have an outcome during the interval
      Cens_followup == (start_interval + 1) ~ Cens_outcome, # They experienced their outcome
      Cens_followup == start_interval ~ 0), # They didn't have the outcome
    
    long_cens_startearly = case_when(
      Cens_followup > (start_interval + 1) ~ 0, # They didn't get censored because of starting the treatment early in that interval
      Cens_followup == (start_interval + 1) ~ 0, # They didn't get censored due to starting treatment early in that interval
      Cens_followup == start_interval ~ Cens_startearly), # They should get the correct reason for censoring
        
    interval_fu = case_when(
      Cens_followup > (start_interval + 1)  ~ 1, # They should get credit for the day of person-time in the interval
      Cens_followup == (start_interval + 1) ~ 1, # They should get credit for the followup time in the interval
      Cens_followup == start_interval ~ 0), # They shouldn't get credit for the person-time in the interval
   
     end_interval = case_when(
      Cens_followup > (start_interval + 1)  ~ start_interval + 1, # The interval will end after 1 day
      Cens_followup == (start_interval + 1) ~ start_interval + 1, # The interval ends at their outcome time
      Cens_followup == start_interval ~ start_interval) # For them, the interval starts and stops on the same day
    ) %>% 
  # For each ID, remove the subsequent rows after Cens_followup = (start_interval + 1) & Cens_startearly == 0,
  # or Cens_followup = start_interval, i.e.
  # (start_interval > Cens_followup - 1 & Cens_startearly == 0), or 
  # start_interval > Cens_followup:
  group_by(ID) %>% 
  filter(!((Cens_followup < (start_interval + 1) & Cens_startearly == 0) | 
                   Cens_followup < start_interval))

nrow(long_trt_30_90_first30days)

  

### start_interval = 30 ### 
long_trt_30_90_interval30 <- trt_30_90_with_cens_no_0_fu %>% 
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>% 
  crossing(start_interval = 30) %>% 

  mutate(date = discharge_date + start_interval,  
         intv_age = as.integer(difftime(date, birthdate, units = "days") / 365.25),  
         
         long_cens_startearly = 0, 
         long_cens_nostart = 0,
         
         long_outcome = case_when(
           Cens_followup > 90 ~ 0, # They shouldn't be counted as experiencing the outcome during this interval
           Cens_followup <= 90 ~ Cens_outcome), # They should receive their true outcome
        
         
         interval_fu = case_when(
           Cens_followup > 90 ~ 60, # They shouldn't be followed past day 90
           Cens_followup <= 90 ~ Cens_followup - 30), # This interval covers as many days as they had past 90
         
         end_interval = case_when(
           Cens_followup > 90 ~ 90, # They shouldn't be followed past day 90
           Cens_followup <= 90 ~ Cens_followup) # Their interval ends at their follow-up time
         ) 
  
  

### start_interval = 90 ### 
long_trt_30_90_interval90 <- trt_30_90_with_cens_no_0_fu %>%
  filter(!(Cens_followup <= 90 & Cens_nostart == 0)) %>%
  # If the above condition is met, don't do the following
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>% 
  crossing(start_interval = 90) %>% 
  mutate(date = discharge_date + start_interval,  
         intv_age = as.integer(difftime(date, birthdate, units = "days") / 365.25),  
         
         long_cens_startearly = 0, # No observations are getting censored for starting early at this point

         long_outcome = case_when(
           Cens_nostart == 1 ~ 0, # They shouldn't be counted as experiencing the outcome during this interval
           Cens_nostart == 0 & Cens_followup > 180 ~ 0, # They didn't have the outcome
           Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_outcome), # They should receive their true outcome
         
         long_cens_nostart = case_when(
           Cens_nostart == 1 ~ 1, # They were censored due to not starting treatment
           Cens_nostart == 0 & Cens_followup > 180 ~ 0, # They were not censored due to not starting treatment
           Cens_nostart == 0 & Cens_followup <= 180 ~ 0), # They were not censored due to not starting treatment
         
         interval_fu = case_when(
           Cens_nostart == 1 ~ 90, # They shouldn't be followed past day 90
           Cens_nostart == 0 & Cens_followup > 180 ~ 90, # This interval covers 90 days
           Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup - 90), # This interval covers as many days as they had past 90
         
         end_interval = case_when(
           Cens_nostart == 1 ~ 90, # They shouldn't be followed past day 90
           Cens_nostart == 0 & Cens_followup > 180 ~ 180, # Their follow-up ends at day 18
           Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup), # Their interval ends at their follow-up time

         recentstart = case_when(
           # This is for convenience and to signify that these people's hypothetical 
           # treatment regimen would involve starting at day 90, rather than earlier
           Cens_nostart == 1 ~ 1,
           # We need to create a "recent start" variable to ensure we only upweight those 
           # who started near the end of the interval. Here, we are requiring them to start within the last 7 days
           Cens_nostart == 0 & RX_start >= 83 ~ 1, 
           Cens_nostart == 0 & RX_start < 83 ~ 0))


# Find people who meet this condition "Cens_followup == start_interval + 1 & Cens_startearly == 0) |
# Cens_followup == start_interval", and hence shouldn't have start_interval = 30 or 90
excluded_ids <- long_trt_30_90_first30days %>%
  filter(
      (Cens_followup == start_interval + 1 & Cens_startearly == 0) |
        Cens_followup == start_interval
      ) %>%
  distinct(ID)

combined_interval30_90 <- bind_rows(long_trt_30_90_interval30, long_trt_30_90_interval90) %>% 
  filter(!(ID %in% excluded_ids$ID))



long_trt_30_90 <- bind_rows(long_trt_30_90_first30days, combined_interval30_90) %>% ungroup()


nrow(long_trt_30_90) 
# 43980


first_30_days <- long_trt_30_90 %>%
  filter(start_interval <= 29)

day_30_to_90 <- long_trt_30_90 %>%
  filter(start_interval == 30)  

day_90_plus <- long_trt_30_90 %>%
  filter(start_interval == 90 & recentstart == 1) 

day_90_plus_not_recent <- long_trt_30_90 %>%
  filter(start_interval == 90 & recentstart == 0) 



nrow(first_30_days) # 41761
nrow(day_30_to_90) # 1201
nrow(day_90_plus) # 765
nrow(day_90_plus_not_recent) # 253




first_30_model <- glm(
  long_cens_startearly ~ intv_age + sex + renal + start_interval + I(start_interval^2), 
  data = first_30_days, 
  family = binomial(link = "logit")
  ) 

summary(first_30_model)

first_30_IPCW <- first_30_days %>% 
  mutate(FU_uncens = 1 - predict(first_30_model, type = "response")) %>% 
  mutate(Interval_IPCW = 1 / FU_uncens)



# In the second data set, everyone's person-time gets an interval weight of 1
# because no one is censored by design during this interval.
day_30_to_90_IPCW <- day_30_to_90 %>%
  mutate(Interval_IPCW = 1)


day_90_plus_filtered <- day_90_plus %>%
  filter(recentstart == 1)

# Fit the logistic regression model
day_90_plus_model <- glm(
  long_cens_nostart ~ intv_age + sex + renal,
  data = day_90_plus_filtered,
  family = binomial(link = "logit")
  )

summary(day_90_plus_filtered$renal)

day_90_plus_FU_uncenes <- day_90_plus_filtered %>% 
  mutate(FU_uncens = 1 - predict(day_90_plus_model, type = "response"))


day_90_plus_interval_IPCW <- day_90_plus_filtered %>%
  mutate(FU_uncens = 1 - predict(day_90_plus_model, type = "response")) %>% 
  bind_rows(day_90_plus_not_recent) %>%
  arrange(ID, start_interval) %>%
  mutate(
    Interval_IPCW = case_when(
      recentstart == 1 & long_cens_nostart == 0 ~ 1 / FU_uncens,  
      recentstart == 0 & long_cens_nostart == 0 ~ 1,               
      TRUE ~ 0 
    )
  )
nrow(day_90_plus_interval_IPCW)

# And finally we can calculate our cumulative IPCW
weights_trt_30_90 <- bind_rows(first_30_IPCW, 
                               day_30_to_90_IPCW, 
                               day_90_plus_interval_IPCW) %>%
  arrange(ID, start_interval) %>%
  mutate(
    Cumulative_IPCW = NA_real_,  # Initialize Cumulative_IPCW
    Last_cumulative_IPCW = NA_real_  # Initialize Last_cumulative_IPCW
  ) 


# Loop through each unique ID to calculate the cumulative IPCW
weights_trt_30_90_cumulativeIPCW <- weights_trt_30_90 %>%
  group_by(ID) %>%
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
    },
    Last_cumulative_IPCW = lag(Cumulative_IPCW, default = NA_real_)
  ) %>%
  ungroup()






wted_trt_30_90 <- weights_trt_30_90_cumulativeIPCW %>%
  group_by(ID, start_interval) %>% 
  filter(Cumulative_IPCW != 0 & interval_fu != 0)

nrow(wted_trt_30_90) # 43052

zeros_30_90 <- weights_trt_30_90_cumulativeIPCW %>%
  group_by(ID, start_interval) %>% 
  filter(Cumulative_IPCW == 0 | interval_fu == 0)

nrow(zeros_30_90) # 928



 

