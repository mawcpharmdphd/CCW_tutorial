

getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(dplyr)
library(survey)

load("/Users/yili/Desktop/Michael CCW project/Stored_data/wted_trt_0_90.Rdata")
load("/Users/yili/Desktop/Michael CCW project/Stored_data/zeros_0_90.Rdata")

# Subset to observations with start_interval equal to 30 and recentstart equal to 1, creating a flag for treated named "treat"
start_90_recentstart_1_trt <- wted_trt_0_90 %>%
  filter(start_interval == 90, recentstart == 1) %>%
  mutate(treat = 1)

# Doing the same subsetting as above to make sure we eliminate any earlier 0-duration intervals, with a 0 for the "treat" flag. 
# We will also give them IPCWs of 1 to make the weighted comparisons a bit easier. 
# This has to be handled a bit differently in the "30_90" analysis where some of these people will have non-zero IPCW.
start_90_recentstart_1_zeros <- zeros_0_90 %>%
  filter(start_interval == 90, recentstart == 1) %>%
  mutate(
    treat = 0,
    Cumulative_IPCW = 1
  )

# Combine the two data sets, keeping covariates, the treatment flag, and cumulative IPCW
interval_90 <- bind_rows(
  start_90_recentstart_1_trt,
  start_90_recentstart_1_zeros
  ) %>%
  select(intv_age, sex, renal, Cumulative_IPCW, treat)

# Now that the cohort is created, we can start estimating SMDs. 
# We want to compare the covariate values in recent initiators with the values in those they will be standing in for (i.e., recent initiators combined with those who were censored).

##### Before weighing #####
# Let's start in the unweighted cohort. 

# Calculate mean and SD by treat group
by_treat <- interval_90 %>%
  group_by(treat) %>%
  summarise(
    age_mean = mean(intv_age, na.rm = TRUE),
    age_stddev = sd(intv_age, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate overall mean and SD
overall <- interval_90 %>%
  summarise(
    treat = NA,
    age_mean = mean(intv_age, na.rm = TRUE),
    age_stddev = sd(intv_age, na.rm = TRUE)
  )

# Combine both
interval_90_mean_stddev_unwt <- bind_rows(by_treat, overall)


# To calculate the SMD, we need to divide the difference between the recent initiators (and the full population) by the SD in the overall cohort
# The easiest way to do this is with some transposing. 
# First, we should replace the "missing" value for treat in the overall with some value (-999).
prep_tpose_interval_90_unwt <- interval_90_mean_stddev_unwt %>%
  mutate(
    treat = if_else(is.na(treat), -999, treat)
  )


# Next, we can transpose to put all the means and SDs into 1 observation
tposed_90_mean_unwt <- prep_tpose_interval_90_unwt %>%
  select(treat, age_mean) %>%
  mutate(treat = as.character(treat)) %>%
  pivot_wider(
    names_from = treat,
    values_from = age_mean,
    names_prefix = "Mean_"
  )

tposed_90_stddev_unwt <- prep_tpose_interval_90_unwt %>%
  select(treat, age_stddev) %>%
  mutate(treat = as.character(treat)) %>%
  pivot_wider(
    names_from = treat,
    values_from = age_stddev,
    names_prefix = "STDDEV_"
  )

# Now we can add flags to merge these sets together, adding the "analysis" flag for means.
tposed_90_mean_flag_unwt <- tposed_90_mean_unwt %>%
  mutate(
    analysis = "unwt"
  )

# Adding the "analysis" flag for standard deviations
tposed_90_stddev_flag_unwt <- tposed_90_stddev_unwt %>%
  mutate(
    analysis = "unwt"
  )

# Now we merge the data and calculate the SMD, comparing the mean in the recent initiators (mean_1) to the mean in everyone (mean_-999)
merge_unwt_analysis <- tposed_90_mean_flag_unwt %>%
  left_join(tposed_90_stddev_flag_unwt, by = "analysis") %>%
  mutate(
    variable = "age",
    SMD = (Mean_1 - `Mean_-999`) / `STDDEV_-999`
  )
#### SMD of age before IPCW
merge_unwt_analysis$SMD
# -0.04804952

# What about binary variables? 
# We need the proportions in the recent initiators as well as the proportions overall.
# Renal by treat
renalprop_unwt_90 <- interval_90 %>%
  count(renal, treat) %>%
  group_by(treat) %>%
  mutate(PCT_COL = 100 * n / sum(n)) %>%
  ungroup()

# Renal overall
all_renalprop_unwt_90 <- interval_90 %>%
  count(renal) %>%
  mutate(PERCENT = 100 * n / sum(n))

# Sex by treat
sexprop_unwt_90 <- interval_90 %>%
  count(sex, treat) %>%
  group_by(treat) %>%
  mutate(PCT_COL = 100 * n / sum(n)) %>%
  ungroup()

# Sex overall
all_sexprop_unwt_90 <- interval_90 %>%
  count(sex) %>%
  mutate(PERCENT = 100 * n / sum(n))

# Next, we limit ourselves to one level of the variables (1 for female, Y for renal) and create flag for the variable name. 
# We also subset the normal tables to treat = 1.
only_Y_renal_unwt_90 <- renalprop_unwt_90 %>%
  filter(renal == "Y", treat == 1) %>%
  mutate(
    variable = "ren",
    PROP_1 = PCT_COL / 100
  )

only_Y_all_renal_unwt_90 <- all_renalprop_unwt_90 %>%
  filter(renal == "Y") %>%
  mutate(
    variable = "ren",
    PROP_all = PERCENT / 100
  )

only_1_sex_unwt_90 <- sexprop_unwt_90 %>%
  filter(sex == "1", treat == 1) %>%
  mutate(
    variable = "fem",
    PROP_1 = PCT_COL / 100
  )

only_1_all_sex_unwt_90 <- all_sexprop_unwt_90 %>%
  filter(sex == "1") %>%
  mutate(
    variable = "fem",
    PROP_all = PERCENT / 100
  )

# Next, we can combine these sets together and calculate SMDs.
cat_var_90_unwt <- bind_rows(
  full_join(only_Y_renal_unwt_90, only_Y_all_renal_unwt_90, by = "variable"),
  full_join(only_1_sex_unwt_90, only_1_all_sex_unwt_90, by = "variable")
  ) %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2)
  )

#### SMD of renal and sex before IPCW
cat_var_90_unwt$SMD
# renal       female sex
# -0.08126770 0.07060821



##### After weighing #####
# What if we want to estimate the SMDs after weighting? 
# We repeat the above by incorporating Cumulative_IPCW.

# Define a function to compute weighted standard deviation
weighted_sd <- function(x, w) {
  wm <- weighted.mean(x, w)
  sqrt(sum(w * (x - wm)^2, na.rm = TRUE) / sum(w, na.rm = TRUE))
}

# Calculate weighted mean and SD for each treatment group
interval_90_mean_stddev_wt <- interval_90 %>%
  group_by(treat) %>%
  summarise(
    age_mean = weighted.mean(intv_age, Cumulative_IPCW),
    age_stddev = weighted_sd(intv_age, Cumulative_IPCW),
    .groups = 'drop'
  )

# Also calculate overall mean and SD
overall_stats <- interval_90 %>%
  summarise(
    treat = NA_real_,  # numeric NA
    age_mean = weighted.mean(intv_age, Cumulative_IPCW),
    age_stddev = weighted_sd(intv_age, Cumulative_IPCW)
  )

# Combine group-level and overall statistics
interval_90_mean_stddev_wt <- bind_rows(interval_90_mean_stddev_wt, overall_stats)

# To calculate the SMD, we need to divide the difference between the two groups by the SD in the overall cohort.
# The easiest way to do this is with some transposing.
# First, we should replace the "missing" value for treat in the overall with -999.
prep_tpose_interval_90_wt <- interval_90_mean_stddev_wt %>%
  mutate(treat = ifelse(is.na(treat), -999, treat))

# Next, we can transpose to put all the means and SDs into 1 observation.
tposed_90_mean_wt <- prep_tpose_interval_90_wt %>%
  select(treat, age_mean) %>%
  pivot_wider(names_from = treat, values_from = age_mean, names_prefix = "Mean_")

tposed_90_stddev_wt <- prep_tpose_interval_90_wt %>%
  select(treat, age_stddev) %>%
  pivot_wider(names_from = treat, values_from = age_stddev, names_prefix = "STDDEV_")

# Now we can add flags to merge these sets together
tposed_90_mean_flag_wt <- tposed_90_mean_wt %>%
  mutate(analysis = "wted") 

tposed_90_stddev_flag_wt <- tposed_90_stddev_wt %>%
  mutate(analysis = "wted") 

# Now we can add flags to merge datasets.
merge_wt_analysis <- full_join(
  tposed_90_mean_flag_wt, tposed_90_stddev_flag_wt, by = "analysis"
  ) %>%
  mutate(
    variable = "age",
    SMD = (Mean_1 - `Mean_-999`) / `STDDEV_-999`
  )
#### SMD of age after IPCW
merge_wt_analysis$SMD
# -0.003969102


# What about binary variables? 
# We need the proportions in the treat = 1 group as well as the overall proportions.
# Renal by treat
renalprop_wt_90 <- interval_90 %>%
  group_by(renal, treat) %>%
  filter(treat == 1) %>% 
  summarise(n = sum(Cumulative_IPCW, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(
    PCT_COL = (n / sum(n, by = treat, na.rm = TRUE)) * 100   
  )


# Renal overall
all_renalprop_wt_90 <- interval_90 %>%
  group_by(renal) %>%
  summarise(
    n = sum(Cumulative_IPCW, na.rm = TRUE), 
    .groups = 'drop'
    ) %>%
  mutate(PERCENT = n / sum(n, na.rm = TRUE) * 100) 

# Sex by treat
sexprop_wt_90 <- interval_90 %>%
  group_by(sex, treat) %>% 
  filter(treat == 1) %>% 
  summarise(
    n = sum(Cumulative_IPCW, na.rm = TRUE), 
    .groups = 'drop'
  ) %>%
  mutate(PCT_COL = 100 * n / sum(n)) 


# Sex overall
all_sexprop_wt_90 <- interval_90 %>%
  group_by(sex) %>%
  summarise(
    n = sum(Cumulative_IPCW),
    .groups = 'drop'
  ) %>%
  mutate(PERCENT = 100 * n / sum(n, na.rm = TRUE)) 

# Next, we limit ourselves to one level of the variables (1 for female, Y for renal) and create flag for the variable name. 
# We also subset to treat = 1
only_Y_renal_wt_90 <- renalprop_wt_90 %>%
  filter(renal == "Y", treat == 1) %>%
  mutate(variable = "ren", PROP_1 = PCT_COL / 100)

only_Y_all_renal_wt_90 <- all_renalprop_wt_90 %>%
  filter(renal == "Y") %>%
  mutate(variable = "ren", PROP_all = PERCENT / 100)

only_1_sex_wt_90 <- sexprop_wt_90 %>%
  filter(sex == "1", treat == 1) %>%
  mutate(variable = "fem", PROP_1 = PCT_COL / 100)

only_1_all_sex_wt_90 <- all_sexprop_wt_90 %>%
  filter(sex == "1") %>%
  mutate(variable = "fem", PROP_all = PERCENT / 100)

# Next, we can combine datasets together and calculate SMDs.
cat_var_90_wt_renal <- only_Y_renal_wt_90 %>%
  full_join(only_Y_all_renal_wt_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 

cat_var_90_wt_fem <- only_1_sex_wt_90 %>%
  full_join(only_1_all_sex_wt_90, by = "variable") %>%
  mutate(
    SMD = (PROP_1 - PROP_all) / sqrt(((PROP_1 * (1 - PROP_1) + PROP_all * (1 - PROP_all)) / 2))
  ) 


cat_var_90_wt_renal$SMD
# 0.00560793
cat_var_90_wt_fem$SMD
# -0.009600364

