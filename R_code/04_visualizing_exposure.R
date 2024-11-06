
getwd()
setwd("/Users/yili/Desktop/Michael CCW project")

library(tidyverse)
library(haven)
library(ggplot2)

daily_trt_0_30 <- wted_trt_0_30 %>%
  # Generate a sequence of days for each observation
  rowwise() %>%
  do({
    data.frame(
      .,
      day = seq(.$start_interval, .$end_interval - 1)
    )
  }) %>%
  ungroup() %>%
  mutate(
    on_treatment = case_when(
      is.na(RX_start) ~ 0,  # If RX_start is missing, not on treatment
      RX_start <= day ~ 1,  # If RX_start is before or on the current day, on treatment
      TRUE ~ 0              # Otherwise, not on treatment
    )
  ) %>%
  filter(interval_fu != 0)


pct_on_treatment_0_30 <- daily_trt_0_30 %>%
  group_by(day) %>% 
  summarise(
    cume_proportion_0_30 = weighted.mean(on_treatment, Cumulative_IPCW, na.rm = TRUE)  # Calculate weighted mean
  ) %>%
  ungroup() 




daily_trt_0_90 <- wted_trt_0_90 %>%
  # Generate a sequence of days for each observation
  rowwise() %>%
  do({
    data.frame(
      .,
      day = seq(.$start_interval, .$end_interval - 1)
    )
  }) %>%
  ungroup() %>%
  mutate(
    on_treatment = case_when(
      is.na(RX_start) ~ 0,  # If RX_start is missing, not on treatment
      RX_start <= day ~ 1,  # If RX_start is before or on the current day, on treatment
      TRUE ~ 0              # Otherwise, not on treatment
    )
  ) %>%
  filter(interval_fu != 0)


pct_on_treatment_0_90 <- daily_trt_0_90 %>%
  group_by(day) %>% 
  summarise(
    cume_proportion_0_90 = weighted.mean(on_treatment, Cumulative_IPCW, na.rm = TRUE)  
  ) %>%
  ungroup() 

pct_on_treatment_0_90


pct_on_trt_0_30_0_90 <- pct_on_treatment_0_90 %>%
  left_join(pct_on_treatment_0_30, by = "day", suffix = c("_0_90", "_0_30")) 



# Assuming pct_on_trt_0_30_0_90 is your merged data frame
ggplot(pct_on_trt_0_30_0_90, aes(x = day)) +
  geom_step(aes(y = cume_proportion_0_30), 
            color = "blue", 
            size = 1.5, 
            alpha = 0.5, 
            linetype = "solid") +  
  geom_step(aes(y = cume_proportion_0_90), 
            color = "red", 
            size = 1.5, 
            alpha = 0.5, 
            linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 1)) +   
  labs(x = "Day", y = "Cumulative Proportion") +  
  theme_minimal()  






# What if we want to try something similar with the 30-90 day individuals? 
# We need to modify the code, as we already have daily observations


daily_trt_30_90 <- wted_trt_30_90 %>%
  filter(interval_fu != 0) %>%
  # Use unnest and map to create daily observations
  rowwise() %>%
  mutate(
    # Create a sequence of days based on start_interval and end_interval
    daily_data = list(if (start_interval < 30) {
      # For start_interval < 30, create a single entry
      data.frame(day = start_interval, 
                 on_treatment = ifelse(!is.na(RX_start) & RX_start <= start_interval, 1, 0))
    } else {
      # For start_interval = 30 or 90, generate daily entries
      tibble(day = seq(start_interval, end_interval - 1),
             on_treatment = map_int(seq(start_interval, end_interval - 1),
                                    ~ ifelse(is.na(RX_start) | (Cens_startearly == 1), 0,
                                             ifelse(RX_start <= ., 1, 0))))
    })
  ) %>%
  # Unnest the daily_data to create the daily treatment observations
  unnest(daily_data) 



pct_on_treatment_30_90 <- daily_trt_30_90 %>%
  group_by(day) %>%
  summarize(
    cume_proportion_30_90 = weighted.mean(on_treatment, Cumulative_IPCW, na.rm = TRUE),
    .groups = 'drop'  # This drops the grouping structure after summarizing
  )

# Now merge all the percentages
pct_on_trt_all <- pct_on_treatment_0_90 %>%
  full_join(pct_on_treatment_0_30, by = "day", suffix = c("_0_90", "_0_30")) %>%
  full_join(pct_on_treatment_30_90, by = "day") %>%
  select(day, cume_proportion_0_90, cume_proportion_0_30, cume_proportion_30_90)



pdf("R/Figure 3. Percentage of initiating treatment.pdf", height = 6, width = 8)
ggplot(data = pct_on_trt_all, aes(x = day)) +
  geom_step(aes(y = cume_proportion_0_30), color = "blue", size = 1, alpha = 0.5, linetype = "solid") +
  geom_step(aes(y = cume_proportion_0_90), color = "red", size = 1, alpha = 0.5, linetype = "dashed") +
  geom_step(aes(y = cume_proportion_30_90), color = "gray", size = 1, alpha = 0.5, linetype = "dotted") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20), 
                     name = "Time since myocardial infarction",
                     labels = scales::number_format()) +
  scale_y_continuous(limits = c(0, 1), 
                     name = "Proportion having initiated treatment") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  theme(legend.position = "none") 

dev.off()

