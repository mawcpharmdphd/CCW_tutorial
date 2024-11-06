
getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)

trt_0_30_with_cens <- read.csv("~/Desktop/Michael CCW project/R/trt_0_30_with_cens.csv")


long_trt_0_30 <- trt_0_30_with_cens %>%
  # Transform Discharge_date to a proper Date object that allows arithmetic with days
  mutate(discharge_date = as.Date(Discharge_date, format = "%Y-%m-%d")) %>%
  
  # Generate two rows per individual with start_interval = 0 and 30
  crossing(start_interval = c(0, 30)) %>%
  
  # Calculate the new date for each interval
  mutate(date = discharge_date + start_interval) %>%
  
  # generate multiple rows for each individual based on intervals
  unnest_longer(start_interval) %>%
  
  # Make sure nobody who has less than 30 days of follow-up gets a second observation
  # Be careful: add "start_interval == 30" at the end (not "start_interval == 0")
  filter(!(Cens_followup <= 30 & Cens_nostart == 0 & start_interval == 30)) %>% 
  
  mutate(date = discharge_date + start_interval,  # Calculate the start date of the interval
    intv_age = as.integer(difftime(date, birthdate, units = "days") / 365.25),  
    
    long_outcome = case_when(
      start_interval == 0 & Cens_followup > 30 ~ 0,
      start_interval == 0 & Cens_followup <= 30 ~ Cens_outcome,
      
      start_interval == 30 & Cens_nostart == 1 ~ 0,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 0,
      start_interval == 30 & Cens_nostart == 0 ~ Cens_outcome
      ),
    
    long_cens_nostart = case_when(
      start_interval == 0 & Cens_followup > 30 ~ 0,
      start_interval == 0 & Cens_followup <= 30 ~ 0,
      
      start_interval == 30 & Cens_nostart == 1 ~ 1,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 0,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ 0,
      ),
    
    interval_fu = case_when(
      start_interval == 0 & Cens_followup > 30 ~ 30,
      start_interval == 0 & Cens_followup <= 30 ~ Cens_followup,
      
      start_interval == 30 & Cens_nostart == 1 ~ 0,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 150,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup - 30
      ),
    
    end_interval = case_when(
      start_interval == 0 & Cens_followup > 30 ~ 30,
      start_interval == 0 & Cens_followup <= 30 ~ Cens_followup,
      start_interval == 30 & Cens_nostart == 1 ~ 30,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup > 180 ~ 180,
      start_interval == 30 & Cens_nostart == 0 & Cens_followup <= 180 ~ Cens_followup
      ),
    
    
    recentstart = case_when(# This is for convenience and to signify that these people's hypothetical treatmeny
                            # regimen would involve starting at day 30, rather than earlier
                            start_interval == 30  & Cens_nostart == 1 ~ 1,
                            # We need to create a "recent start" variable to ensure we only upweight those who started near 
                            # the end of the interval (e.g. within the last 7 days)
                            start_interval == 30  & Cens_nostart == 0 & RX_start >= 23 ~ 1,
                            start_interval == 30  & Cens_nostart == 0 & RX_start < 23 ~ 0)
    ) 


  


# Split this long data set into 3 different data sets.
first_30_days <- long_trt_0_30 %>% filter(start_interval == 0)

other_days <- long_trt_0_30 %>% filter(start_interval == 30 & recentstart == 1)

other_days_not_recent <- long_trt_0_30 %>%
  filter(!(start_interval == 0 | (start_interval == 30 & recentstart == 1)))

# For the first_30_weights dataset, assign 1 to IPCW
first_30_weights <- first_30_days %>% mutate(Cumulative_IPCW = 1)

# For the second data set, we need to fit a logistic regression model predicting the probability 
# of being uncensored at the start of the interval among those with recentstart=1 
# (i.e., the people who were censored at day 30 and the people who started treatment from day 23 to day 30.
    
model <- glm(
  long_cens_nostart ~ intv_age + sex + renal,
  data = other_days,
  family = binomial(link = "logit")
)

summary(model)

other_days_for_IPCW <- other_days %>%
  mutate(FU_uncens = 1 - predict(model, type = "response")) # prob of *uncensored*


# Generate the IPCWs
other_days_weights <- bind_rows(other_days_for_IPCW, other_days_not_recent) %>%
  arrange(ID, start_interval)  %>% # sort by ID and start_interval
  mutate(
    Cumulative_IPCW = case_when(
      recentstart == 1 & long_cens_nostart == 0 ~ 1 / FU_uncens,  # Recently started and uncensored
      recentstart == 0 & long_cens_nostart == 0 ~ 1,              # Didn't start recently, uncensored
      TRUE ~ 0                                                    
    )
  )



combined_data <- bind_rows(other_days_weights, first_30_weights) %>%
  arrange(ID, start_interval)  

# Non-zero weights:
wted_trt_0_30 <- combined_data %>%
  filter(Cumulative_IPCW != 0) 

# Zero weights
zeros_0_30 <- combined_data %>%
  filter(Cumulative_IPCW == 0)  

