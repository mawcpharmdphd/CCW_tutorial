

getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(dplyr)
library(survey)

load("/Users/yili/Desktop/Michael CCW project/Stored_data/wted_trt_30_90.Rdata")
load("/Users/yili/Desktop/Michael CCW project/Stored_data/zeros_30_90.Rdata")

# Subset to observations with start_interval equal to 90 and recentstart equal to 1, creating a flag for treated named "treat". 
# Because there were already censoring weights applied before day 90, we also need to create an "Unweighted" variable using the previous IPCW value for our "unweighted" analysis.
  
start_30_90_recentstart_1_trt <- wted_trt_30_90 %>%
  filter(start_interval == 90, recentstart == 1) %>%
  mutate(treat = 1,
         Unweighted = Last_cumulative_IPCW)

# Doing the same subsetting as above to make sure we eliminate any earlier 0-duration intervals, with a 0 for the "treat" flag. 
# We also set values for the "Unweighted" variable and the "Cumulative IPCW" variable to the last_cumulative_IPCW to ensure we are only examining the impact of the day 90 "failed to start treatment" weights.
  
start_30_90_recentstart_1_zeros <- zeros_30_90 %>%
  filter(start_interval == 90, recentstart == 1) %>%
  mutate(
    treat = 0,
    Cumulative_IPCW = 1,
    Unweighted = Last_cumulative_IPCW
  ) 

# Combine the two data sets, keeping covariates, the treatment flag, and cumulative IPCW
interval_30_90 <- bind_rows(
  start_30_90_recentstart_1_trt,
  start_30_90_recentstart_1_zeros
  ) %>%
  select(intv_age, sex, renal, Cumulative_IPCW, treat, Unweighted)

# drop columns `ID`, `start_interval`:
interval_30_90 <- interval_30_90[, -c(1:2)]

# Now that the cohort is created, we can start estimating SMDs. 

##### Before weighing #####
# Let's start in the unweighted cohort. 
# First, let's get the means and SD using the "Unweighted" weights.

# Define a function to compute weighted SD
weighted_sd <- function(x, w) {
  wm <- weighted.mean(x, w)
  sqrt(sum(w * (x - wm)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
}

# Calculate weighted mean and SD for each treatment group
interval_30_90_mean_stddev_unwt <- interval_30_90 %>%
  group_by(treat) %>%
  summarise(
    age_mean = weighted.mean(intv_age, Unweighted),
    age_stddev = weighted_sd(intv_age, Unweighted),
    .groups = 'drop'
  )

# Also calculate overall mean and SD
overall_stats <- interval_30_90 %>%
  summarise(
    treat = NA_real_,  # numeric NA
    age_mean = weighted.mean(intv_age, Unweighted),
    age_stddev = weighted_sd(intv_age, Unweighted)
  )

# Combine group-level and overall statistics
interval_30_90_mean_stddev_unwt <- bind_rows(interval_30_90_mean_stddev_unwt, overall_stats)

# To calculate the SMD, we need to divide the difference between the two groups by the standard deviation in the overall cohort
# The easiest way to do this is with some transposing.
# First, we should replace the "missing" value for treat in the overall with some value (-999).
prep_tpose_interval_30_90_unwt <- interval_30_90_mean_stddev_unwt %>%
  mutate(treat = ifelse(is.na(treat), -999, treat))

# Next, we can transpose to put all the means and SDs into 1 observation
tposed_30_90_mean_unwt <- prep_tpose_interval_30_90_unwt %>%
  select(treat, age_mean) %>%
  pivot_wider(names_from = treat, values_from = age_mean, names_prefix = "Mean_")

tposed_30_90_stddev_unwt <- prep_tpose_interval_30_90_unwt %>%
  select(treat, age_stddev) %>%
  pivot_wider(names_from = treat, values_from = age_stddev, names_prefix = "STDDEV_")

# Now we can add flags to merge these datasets.
tposed_30_90_mean_flag_unwt <- tposed_30_90_mean_unwt %>%
  mutate(analysis = "unwted") 

tposed_30_90_stddev_flag_unwt <- tposed_30_90_stddev_unwt %>%
  mutate(analysis = "unwted") 

# Now we merge the data and calculate the SMD
merge_unwt_analysis <- full_join(
  tposed_30_90_mean_flag_unwt, tposed_30_90_stddev_flag_unwt, by = "analysis"
) %>%
  mutate(
    variable = "age",
    SMD = (Mean_1 - `Mean_-999`) / `STDDEV_-999`
  )
#### SMD of age after IPCW
merge_unwt_analysis$SMD
# -0.05334684





# What about binary variables? 
# We need the proportions in the treat = 1 group as well as the overall proportions.
# Renal by treat
renalprop_unwt_30_90 <- interval_30_90 %>%
  group_by(renal, treat) %>%
  filter(treat == 1) %>% 
  summarise(n = sum(Unweighted, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(
    PCT_COL = (n / sum(n, by = treat, na.rm = TRUE)) * 100  
  )


# Renal overall
all_renalprop_unwt_30_90 <- interval_30_90 %>%
  group_by(renal) %>%
  summarise(
    n = sum(Unweighted, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(PERCENT = n / sum(n, na.rm = TRUE) * 100) 

# Sex by treat
sexprop_unwt_30_90 <- interval_30_90 %>%
  group_by(sex, treat) %>% 
  filter(treat == 1) %>% 
  summarise(
    n = sum(Unweighted, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(PCT_COL = 100 * n / sum(n)) 


# Sex overall
all_sexprop_unwt_30_90 <- interval_30_90 %>%
  group_by(sex) %>%
  summarise(
    n = sum(Unweighted),
    .groups = 'drop'
  ) %>%
  mutate(PERCENT = 100 * n / sum(n, na.rm = TRUE)) 

# Next, we limit ourselves to one level of the variables (1 for female, Y for renal) and create flag for the variable name. 
# We also subset the normal tables to treat = 1
# only_Y_renal_unwt_90
only_Y_renal_unwt_30_90 <- renalprop_unwt_30_90 %>%
  filter(renal == "Y", treat == 1) %>%
  mutate(variable = "ren", PROP_1 = PCT_COL / 100)

# only_Y_all_renal_unwt_90
only_Y_all_renal_unwt_30_90 <- all_renalprop_unwt_30_90 %>%
  filter(renal == "Y") %>%
  mutate(variable = "ren", PROP_all = PERCENT / 100)

# only_1_sex_unwt_90
only_1_sex_unwt_30_90 <- sexprop_unwt_30_90 %>%
  filter(sex == "1", treat == 1) %>%
  mutate(variable = "fem", PROP_1 = PCT_COL / 100)

# only_1_all_sex_unwt_90
only_1_all_sex_unwt_30_90 <- all_sexprop_unwt_30_90 %>%
  filter(sex == "1") %>%
  mutate(variable = "fem", PROP_all = PERCENT / 100)

# Next, we can combine datasets together and calculate SMDs
# Merging the datasets and calculating SMD
cat_var_30_90_unwt_renal <- only_Y_renal_unwt_30_90 %>%
  full_join(only_Y_all_renal_unwt_30_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 

cat_var_30_90_unwt_fem <- only_1_sex_unwt_30_90 %>%
  full_join(only_1_all_sex_unwt_30_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 


cat_var_30_90_unwt_renal$SMD
# -0.09705024
cat_var_30_90_unwt_fem$SMD
# 0.06572475










##### After weighing #####
# What if we want to estimate the standardized mean differences after weighting? 
# We repeat the above by replacing "Unweighted" with "Cumulative_IPCW".

interval_30_90_mean_stddev_wt <- interval_30_90 %>%
  group_by(treat) %>%
  summarise(
    age_mean = weighted.mean(intv_age, Cumulative_IPCW),
    age_stddev = weighted_sd(intv_age, Cumulative_IPCW),
    .groups = 'drop'
  )

overall_stats <- interval_90 %>%
  summarise(
    treat = NA_real_,  
    age_mean = weighted.mean(intv_age, Cumulative_IPCW),
    age_stddev = weighted_sd(intv_age, Cumulative_IPCW)
  )

interval_30_90_mean_stddev_wt <- bind_rows(interval_30_90_mean_stddev_wt, overall_stats)


prep_tpose_interval_30_90_wt <- interval_30_90_mean_stddev_wt %>%
  mutate(treat = ifelse(is.na(treat), -999, treat))

tposed_30_90_mean_wt <- prep_tpose_interval_30_90_wt %>%
  select(treat, age_mean) %>%
  pivot_wider(names_from = treat, values_from = age_mean, names_prefix = "Mean_")

tposed_30_90_stddev_wt <- prep_tpose_interval_30_90_wt %>%
  select(treat, age_stddev) %>%
  pivot_wider(names_from = treat, values_from = age_stddev, names_prefix = "STDDEV_")

tposed_30_90_mean_flag_wt <- tposed_30_90_mean_wt %>%
  mutate(analysis = "wted") 

tposed_30_90_stddev_flag_wt <- tposed_30_90_stddev_wt %>%
  mutate(analysis = "wted") 

merge_wt_analysis <- full_join(
  tposed_30_90_mean_flag_wt, tposed_30_90_stddev_flag_wt, by = "analysis"
  ) %>%
  mutate(
    variable = "age",
    SMD = (Mean_1 - `Mean_-999`) / `STDDEV_-999`
  )
#### SMD of age after IPCW
merge_wt_analysis$SMD
# -0.01636889


# What about binary variables? 
# Renal by treat
renalprop_wt_30_90 <- interval_30_90 %>%
  group_by(renal, treat) %>%
  filter(treat == 1) %>% 
  summarise(n = sum(Cumulative_IPCW, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(
    PCT_COL = (n / sum(n, by = treat, na.rm = TRUE)) * 100  
  )


# Renal overall
all_renalprop_wt_30_90 <- interval_30_90 %>%
  group_by(renal) %>%
  summarise(
    n = sum(Cumulative_IPCW, na.rm = TRUE), 
    .groups = 'drop'
    ) %>%
  mutate(PERCENT = n / sum(n, na.rm = TRUE) * 100) 

# Sex by treat
sexprop_wt_30_90 <- interval_30_90 %>%
  group_by(sex, treat) %>% 
  filter(treat == 1) %>% 
  summarise(
    n = sum(Cumulative_IPCW, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(PCT_COL = 100 * n / sum(n)) 


# Sex overall
all_sexprop_wt_30_90 <- interval_30_90 %>%
  group_by(sex) %>%
  summarise(
    n = sum(Cumulative_IPCW),
    .groups = 'drop'
  ) %>%
  mutate(PERCENT = 100 * n / sum(n, na.rm = TRUE)) 


# only_Y_renal_wt_90
only_Y_renal_wt_30_90 <- renalprop_wt_30_90 %>%
  filter(renal == "Y", treat == 1) %>%
  mutate(variable = "ren", PROP_1 = PCT_COL / 100)

# only_Y_all_renal_wt_90
only_Y_all_renal_wt_30_90 <- all_renalprop_wt_30_90 %>%
  filter(renal == "Y") %>%
  mutate(variable = "ren", PROP_all = PERCENT / 100)

# only_1_sex_wt_90
only_1_sex_wt_30_90 <- sexprop_wt_30_90 %>%
  filter(sex == "1", treat == 1) %>%
  mutate(variable = "fem", PROP_1 = PCT_COL / 100)

# only_1_all_sex_wt_90
only_1_all_sex_wt_30_90 <- all_sexprop_wt_30_90 %>%
  filter(sex == "1") %>%
  mutate(variable = "fem", PROP_all = PERCENT / 100)


cat_var_30_90_wt_renal <- only_Y_renal_wt_30_90 %>%
  full_join(only_Y_all_renal_wt_30_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 

cat_var_30_90_wt_fem <- only_1_sex_wt_30_90 %>%
  full_join(only_1_all_sex_wt_30_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 


cat_var_30_90_wt_renal$SMD
# 0.01224384
cat_var_30_90_wt_fem$SMD
# -0.01409816

