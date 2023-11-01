## EFFECT SIZE TRANSFORMATIONS FOR YOUTH HOMELESSNESS SYSTEMATIC REVIEW

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)
library(esc)

## READ DATA

data_extraction_sheet_location <- "./inputs/data_extraction_sheet.xlsx"
data_extraction_sheet <- read_excel(
  path = data_extraction_sheet_location,
  sheet = "outcome_data",
  col_names = TRUE
)

## CLEAN DATA

clean_data_extraction_sheet <- data_extraction_sheet %>%
  # remove instructions
  filter(!row_number() == 1) %>%
  # remove superfluous columns
  select(
    -primary_extractor,
    -secondary_extractor,
    -meta,
    -required_information,
    -comment,
    -additional_extraction_items
  ) %>%
  # specify variable types
  mutate(
    across(
      c(
        outcome_timing,
        grp1n,
        grp2n,
        prop1event,
        prop2event,
        grp1m,            
        grp1sd,
        grp1se,
        grp2m,
        grp2sd,
        grp2se,
        gain1mean,
        gain1se,          
        gain2mean,
        gain2se,
        or,
        f,
        se,
        totaln,
        b,                
        beta, 
        sdy,
        chisq,
        t,
        t_pvalue,
        mean_diff,
        mean_diff_lower,  
        mean_diff_upper,
        mean_diff_se
        ),
      as.numeric
    )
  ) %>%
  # exclude baseline measurements
  filter(
    !outcome_timing == 0
  )

## GROUP DATA BY ES TYPE

mean_gain <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean Gain"
  ) %>%
  mutate(
    id = row_number()
  )

mean_difference <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean Difference"
  ) %>%
  mutate(
    id = row_number()
  )

mean_sd <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean SD"
  ) %>%
  mutate(
    id = row_number()
  )

mean_se <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean SE"
  ) %>%
  mutate(
    id = row_number()
  )

odds_ratio <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Odds Ratio"
  ) %>%
  mutate(
    totaln = grp1n + grp2n,
    id = row_number()
  )

binary_proportions <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Binary proportions"
  ) %>%
  mutate(
    id = row_number()
  )

## TRANSFORM RESULTS REPORTED AS MEAN GAINS TO HEDGES' G USING ESC PACKAGE

mean_gain_hedges_g <- mean_gain %>%
  # transform reported se to sd for function to work its magic
  mutate(
    gain1sd = gain1se * sqrt(grp1n),
    gain2sd = gain2se * sqrt(grp2n)
  ) %>%
  # input correlation coefficient
  mutate(
    grp1r = 0.7,
    grp2r = 0.7
  ) %>%
  # run es function
  effect_sizes(
    study = study_id,
    fun = "esc_mean_gain",                                              
    grp1n = grp1n,
    gain1mean = gain1mean,
    gain1sd = gain1sd,
    grp1r = grp1r,
    grp2r = grp2r,
    grp2n = grp2n,
    gain2mean = gain2mean,
    gain2sd = gain2sd,
    es.type = "g") %>%
  # rename var
  rename(
    study_id = study,
    es_ci_low = ci.lo,
    es_ci_high = ci.hi,
    sample_size = sample.size,
  ) %>%
  # add row number as id
  mutate(
    id = row_number()
  ) %>%
  # merge back with raw data
  left_join(
    mean_gain,
    by = c(
      "id",
      "study_id")
  ) %>%
  # select vars for reporting
  select(
    study_id,
    outcome_domain,
    outcome_construct,
    outcome_measure,
    outcome_timing,
    estimand,
    es,
    es_ci_low,
    es_ci_high,
    sample_size
  ) 

## TRANSFORM RESULTS REPORTED AS MEAN DIFFERENCES TO HEDGES' G USING CUSTOM FUNCTION

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported mean differences
esc_mean_difference <- function(mean_difference, mean_diff_se = NULL, mean_diff_ci_lower = NULL, mean_diff_ci_upper = NULL, treatment_n, comparison_n, esc_type) {
  # Use reported standard error if provided, otherwise calculate standard error from confidence intervals
  if (!is.null(mean_diff_se)) {
    standard_error <- mean_diff_se
  } else if (!is.null(mean_diff_ci_lower) & !is.null(mean_diff_ci_upper)) {
    standard_error <- (mean_diff_ci_upper - mean_diff_ci_lower) / (2 * 1.96)
  } else {
    stop("Either mean_diff_se or both mean_diff_ci_lower and mean_diff_ci_upper must be provided.")
  }
  
  # Estimate the pooled standard deviation using the standard error
  standard_deviation_pooled <- standard_error / sqrt((1 / treatment_n) + (1 / comparison_n))
  
  # Calculate Cohen's d
  cohens_d <- mean_difference / standard_deviation_pooled
  
  # Calculate the standard error for Cohen's d
  standard_error_d <- standard_error / standard_deviation_pooled
  
  # Calculate the confidence interval for Cohen's d (95% confidence level, Z = 1.96)
  mean_diff_ci_lower_d <- cohens_d - (1.96 * standard_error_d)
  mean_diff_ci_upper_d <- cohens_d + (1.96 * standard_error_d)
  
  # Calculate the t-statistic
  t_stat <- mean_difference / standard_error
  
  # Calculate degrees of freedom
  df <- treatment_n + comparison_n - 2
  
  # Calculate the p-value using the t-distribution
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  
  # Check if Hedges' g is requested
  if (esc_type == 'g') {
    # Calculate Hedges' g
    hedges_g <- cohens_d * (1 - (3 / (4 * (treatment_n + comparison_n) - 9)))
    return(list(effect_size = hedges_g, conf_interval = c(mean_diff_ci_lower_d, mean_diff_ci_upper_d), p_value = p_value))
  } else {
    return(list(effect_size = cohens_d, conf_interval = c(mean_diff_ci_lower_d, mean_diff_ci_upper_d), p_value = p_value))
  }
}

mean_difference_hedges_g <- mean_difference %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_mean_difference(
      mean_difference = mean_diff,
      mean_diff_ci_lower = mean_diff_lower,
      mean_diff_ci_upper = mean_diff_upper,
      treatment_n = grp1n,
      comparison_n = grp2n,
      esc_type = "g"
    )
  )) %>%
  ungroup() %>%
  unnest_wider(
    result
  ) %>%
  # Convert the character column to a list of numeric vectors
  mutate(
    conf_interval = as.character(conf_interval),
    conf_interval = str_extract_all(conf_interval, "[0-9.]+") %>% 
      map(~ as.numeric(.))
  ) %>%
  # Split the list-column into two new columns
  unnest_wider(
    conf_interval, 
    names_sep = "_") %>%
  rename(
    es = effect_size,
    es_ci_low = conf_interval_1,
    es_ci_high = conf_interval_2
  ) %>%
  # add sample size var
  mutate(
    sample_size = grp1n + grp2n
  ) %>%
  # select vars for reporting
  select(
    study_id,
    outcome_domain,
    outcome_construct,
    outcome_measure,
    outcome_timing,
    estimand,
    es,
    es_ci_low,
    es_ci_high,
    sample_size
  ) 

## TRANSFORM RESULTS REPORTED AS MEAN & SD TO HEDGES' G USING ESC PACKAGE

mean_sd_hedges_g <- mean_sd %>%
  # run es function
  effect_sizes(
    study = study_id,
    fun = "esc_mean_sd",                                              
    grp1m = grp1m,
    grp1sd = grp1sd,
    grp1n = grp1n,
    grp2m = grp2m,
    grp2sd = grp2sd,
    grp2n = grp2n,
    es.type = "g") %>%
  # rename var
  rename(
    study_id = study,
    es_ci_low = ci.lo,
    es_ci_high = ci.hi,
    sample_size = sample.size,
  ) %>%
  # add row number as id
  mutate(
    id = row_number()
  ) %>%
  # merge back with raw data
  left_join(
    mean_sd,
    by = c(
      "id",
      "study_id")
  ) %>%
  # select vars for reporting
  select(
    study_id,
    outcome_domain,
    outcome_construct,
    outcome_measure,
    outcome_timing,
    estimand,
    es,
    es_ci_low,
    es_ci_high,
    sample_size
  ) 

## TRANSFORM RESULTS REPORTED AS MEAN & SE TO HEDGES' G USING ESC PACKAGE

# Stop! Transformation not possible as SE or SD not reported

## TRANSFORM RESULTS REPORTED AS BINARY PROPORTIONS TO HEDGES' G USING ESC PACKAGE

binary_proportions_hedges_g <- binary_proportions %>%
  # run es function
  effect_sizes(
    study = study_id,
    fun = "esc_bin_prop",                                              
    prop1event = prop1event,
    grp1n = grp1n,
    prop2event = prop2event,
    grp2n = grp2n,
    es.type = "g") %>%
  # rename var
  rename(
    study_id = study,
    es_ci_low = ci.lo,
    es_ci_high = ci.hi,
    sample_size = sample.size,
  ) %>%
  # add row number as id
  mutate(
    id = row_number()
  ) %>%
  # merge back with raw data
  left_join(
    binary_proportions,
    by = c(
      "id",
      "study_id")
  ) %>%
  # select vars for reporting
  select(
    study_id,
    outcome_domain,
    outcome_construct,
    outcome_measure,
    outcome_timing,
    estimand,
    es,
    es_ci_low,
    es_ci_high,
    sample_size
  ) 

## TRANSFORM RESULTS REPORTED AS ODDS RATIOS TO HEDGES' G USING ESC PACKAGE

odds_ratio_g_one <- odds_ratio %>%
  filter(esc_type == "Odds Ratio") %>%
  convert_or2d(
    study = .$study_id[1],
    or = .$or[1],
    se = .$se[1],
    totaln = .$totaln[1],
    es.type = "g") %>%
  as.data.frame()

odds_ratio_g_two <- odds_ratio %>%
  filter(esc_type == "Odds Ratio") %>%
  convert_or2d(
    study = .$study_id[2],
    or = .$or[2],
    se = .$se[2],
    totaln = .$totaln[2],
    es.type = "g") %>%
  as.data.frame()

odds_ratio_g_three <- odds_ratio %>%
  filter(esc_type == "Odds Ratio") %>%
  convert_or2d(
    study = .$study_id[3],
    or = .$or[3],
    se = .$se[3],
    totaln = .$totaln[3],
    es.type = "g") %>%
  as.data.frame()

odds_ratio_hedges_g <- bind_rows(
  odds_ratio_g_one,
  odds_ratio_g_two,
  odds_ratio_g_three) %>%
  # rename var
  rename(
    study_id = study,
    es_ci_low = ci.lo,
    es_ci_high = ci.hi,
    sample_size = sample.size,
  ) %>%
  # add row number as id
  mutate(
    id = row_number()
  ) %>%
  # add row number as id
  mutate(
    id = row_number()
  ) %>%
  # merge back with raw data
  left_join(
    odds_ratio,
    by = c(
      "id",
      "study_id")
  ) %>%
  # select vars for reporting
  select(
    study_id,
    outcome_domain,
    outcome_construct,
    outcome_measure,
    outcome_timing,
    estimand,
    es,
    es_ci_low,
    es_ci_high,
    sample_size
  ) 

## CLEAN UP DATA FOR EXPORT

effect_sizes <- bind_rows(
  mean_gain_hedges_g,
  mean_difference_hedges_g,
  mean_sd_hedges_g,
  binary_proportions_hedges_g,
  odds_ratio_hedges_g
  ) %>%
  # round quant outcomes for reporting and combine in single column
  mutate(
    es = round(es, 2),
    es_ci_low = round(es_ci_low, 2),
    es_ci_high = round(es_ci_high, 2)
  )