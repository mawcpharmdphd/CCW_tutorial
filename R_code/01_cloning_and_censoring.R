

getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(vetr)

baseccwcohort <- read_sas("baseccwcohort.sas7bdat")


###### CLONING #####
# Next, let's clone individuals into four different data sets representing four potential treatment regimens:
# Treatment 0 to 30 days after MI (trt0_30)
# Treatment 0 to 90 days after MI (trt0_90)
# Treatment 30 to 90 days after MI (trt30_90)
# No treatment after MI (notrt), separate from the main regimens in the manuscript*/

trt0_30 <- baseccwcohort %>%
  mutate(t0censnotrt = case_when(RX_start == 0 ~ 1, 
                                 TRUE ~ 0),
         t0cens30_90 = case_when(RX_start == 0 ~ 1, 
                                 TRUE ~ 0))

trt0_90 <- trt0_30
trt30_90 <- trt0_30
notrt <- trt0_30


# Let's do the trt0_30 cohort first. We need to censor everyone who doesn't start by day 30 at day 30*
# This is directly translated from the SAS code
trt_0_30_with_cens <- trt0_30 %>%
  # First, let's sort out the people with less than 30 days of followup. They should use their original values
  mutate(Cens_followup = case_when(followup < 31 ~ followup,
                                   is.na(RX_start) | RX_start > 30 ~ 30,
                                   RX_start < 31 ~ followup
                                   ),
         Cens_outcome = case_when(followup < 31 ~ outcome,
                                  is.na(RX_start) | RX_start > 30 ~ 0,
                                  RX_start < 31 ~ outcome
                                  ),
  Cens_nostart = case_when( # We also want to make it clear this is a separate censoring mechanism 
                            # if we decided to weight for it
                            followup < 31 ~ 0,
                            # This flag makes it clear they were censored for not starting
                            is.na(RX_start) | RX_start > 30 ~ 1,
                            RX_start < 31 ~ 0)
  ) 



trt_0_90_with_cens <- trt0_90 %>%
  mutate(Cens_followup = case_when(followup < 91 ~ followup,
                                   is.na(RX_start) | RX_start > 90 ~ 90,
                                   RX_start < 91 ~ followup
                                   ),
  Cens_outcome = case_when(followup < 91 ~ outcome,
                           is.na(RX_start) | RX_start > 90 ~ 0,
                           RX_start < 91 ~ outcome
  ),
  Cens_nostart = case_when( 
    followup < 91 ~ 0,
    is.na(RX_start) | RX_start > 90 ~ 1,
    RX_start < 91 ~ 0)
  ) 






trt_30_90_with_cens <- trt30_90 %>%
  mutate(Cens_followup = case_when( # First, let's deal with censoring the people who start within the first 30 days.
                                   RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ RX_start,
                                   RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ followup,
                                   
                                   # Next, we need to deal with the people who have less than 91 days of followup. 
                                   # Their followup should be unchanged and they should not get a flag for either censoring reason
                                   followup < 91 ~ followup,
                                   
                                   # Next, let's deal with people who never start or start after day 90. 
                                   # Their censoring date should be day 90, and their reason should be not starting
                                   is.na(RX_start) | RX_start > 90 ~ 90,
                                   
                                   # /*Now, we need to create the follow-up for people who started an RX within the window
                                   TRUE ~ followup
                                   ),
    Cens_outcome = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 0,
                             RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ outcome,
                             followup < 91 ~ outcome,
                             is.na(RX_start) | RX_start > 90 ~ 0,
                             TRUE ~ outcome),
    Cens_startearly = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 1,
                                RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ 0,
                                followup < 91 ~ 0,
                                is.na(RX_start) | RX_start > 90 ~ 0,
                                TRUE ~ 0),
    Cens_nostart = case_when(RX_start < 30 & !is.na(RX_start) & followup >= RX_start ~ 0,
                             RX_start < 30 & !is.na(RX_start) & followup < RX_start ~ 0,
                             followup < 91 ~ 0,
                             is.na(RX_start) | RX_start > 90 ~ 1,
                             TRUE ~ 0)
    )


notrt_with_cens <- notrt %>%
  mutate(Cens_followup = case_when(is.na(RX_start) ~ followup,
                                   !is.na(RX_start) & followup >= RX_start ~ RX_start,
                                   TRUE ~ followup
                                   ),
    Cens_outcome = case_when(is.na(RX_start) ~ outcome,
                             !is.na(RX_start) & followup >= RX_start ~ 0,
                             TRUE ~ outcome
                             ),
    Cens_start = case_when(is.na(RX_start) ~ 0,
                           !is.na(RX_start) & followup >= RX_start ~ 1,
                           TRUE ~ 0
                           )
    )


write_csv(trt_0_30_with_cens, "R/trt_0_30_with_cens.csv")
write_csv(trt_0_90_with_cens, "R/trt_0_90_with_cens.csv")
write_csv(trt_30_90_with_cens, "R/trt_30_90_with_cens.csv")
write_csv(notrt_with_cens, "R/notrt_with_cens.csv")






