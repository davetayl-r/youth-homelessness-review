## ESTIMATE CORRELATION COEFFICIENT FORkidd_2020 FROM slesnick_2023 AND thulien_2022

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)

## READ DATA

data_extraction_sheet_location <- "./inputs/data_extraction_sheet.xlsx"
data_extraction_sheet <- read_excel(
  path = data_extraction_sheet_location,
  sheet = "outcome_data",
  col_names = TRUE
)

## SUBSET REQUIRED DATA

mean_sd_data <- data_extraction_sheet %>%
  # get data for slesnick_2023 & thulien_2022
  filter(
    study_id %in% c(
      "slesnick_2023",
      "thulien_2022"
    )
  ) %>%
  # filter mean and se data
  filter(
    esc_type == "Mean SD"
  ) %>%
  # select required data
  select(
    study_id,
    outcome_timing,
    outcome_measure,
    grp1n, 
    grp2n, 
    grp1m, 
    grp2m, 
    grp1sd, 
    grp2sd,
    mean_diff,
    mean_diff_lower,
    mean_diff_upper
  ) %>%
  # ensure all data are numeric
  mutate(
    across(
      c(
        outcome_timing,
        grp1n, 
        grp2n, 
        grp1m, 
        grp2m, 
        grp1sd, 
        grp2sd,
        mean_diff,
        mean_diff_lower,
        mean_diff_upper
    ),
    as.numeric)
  ) %>%
  # find the min and max measures of outcome timing
  group_by(
    study_id,
    outcome_measure
  ) %>%
  mutate(
    group_id = row_number()
  ) %>%
  mutate(
    min_flag = case_when(
      group_id == min(group_id) ~ 1,
      TRUE ~ 0
    ),
    max_flag = case_when(
      group_id == max(group_id) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # drop the vars no longer required
  mutate(
    keep_flag = case_when(
      min_flag == 1 | max_flag == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(
    keep_flag == 1
  ) %>%
  # create min/max timing var
  mutate(
    timing = case_when(
      min_flag == 1 ~ "baseline",
      max_flag == 1 ~ "follow_up"
    )
  ) %>%
  # drop vars no longer required
  ungroup() %>%
  select(
    -group_id,
    -min_flag,
    -max_flag,
    -keep_flag,
    -outcome_timing
  )

## TRANSFORM DATA

# Reshape the data to have one row per outcome measure per study, with separate columns for baseline and follow-up values

# Separate data into baseline and follow-up
baseline_mean_sd_data <- mean_sd_data %>% 
  filter(
    timing == "baseline"
  )

follow_up_mean_sd_data <- mean_sd_data %>% 
  filter(
    timing == "follow_up"
  )

# Join the baseline and follow-up data together
reshaped_mean_sd_data <- baseline_mean_sd_data %>%
  left_join(
    follow_up_mean_sd_data,
    by = c("study_id", "outcome_measure"),
    suffix = c("_baseline", "_followup")
  )

# estimate sd of change score
mean_sd_data_est_sd <- reshaped_mean_sd_data %>%
  mutate(
    se_delta_est = (mean_diff_upper_followup - mean_diff_lower_followup)/(2 * 1.96),
    sd_delta_est = se_delta_est * sqrt(grp1m_followup + grp1m_baseline)
  )

# Declare function to calculate the correlation coefficient
compute_r <- function(sd_baseline, sd_followup, sd_change) {
  if(any(!is.numeric(c(sd_baseline, sd_followup, sd_change)))) {
    stop("All input values must be numeric")
  }
  
  denominator <- 2 * sd_baseline * sd_followup
  if (denominator == 0) {
    return(NA)
  }
  
  numerator <- sd_baseline^2 + sd_followup^2 - sd_change^2
  r <- numerator / denominator
  return(r)
}

# Step 3: Apply the function to calculate the correlation coefficient for each group and each outcome measure
correlation_df <- mean_sd_data_est_sd %>%
  rowwise() %>%
  # Apply function
  mutate(
    grp1r = compute_r(grp1sd_baseline, grp1sd_followup, sd_delta_est),
    grp2r = compute_r(grp2sd_baseline, grp2sd_followup, sd_delta_est)
  ) %>%
  # Select required data
  select(
    study_id,
    timing_baseline,
    outcome_measure,
    grp1r,
    grp2r
  )

# Now, correlation_df contains the correlation coefficient for each group and each outcome measure
# Merge this back with the original data if needed, or proceed to use these correlation coefficients for further analysis

# Optionally, merge the computed correlation coefficients back with the original data
merged_df <- df %>%
  left_join(
    correlation_df %>% select(study_id, outcome_measure, grp1r, grp2r),
    by = c("study_id", "outcome_measure")
  )