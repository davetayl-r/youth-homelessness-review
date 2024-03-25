## EFFECT SIZE TRANSFORMATIONS FOR YOUTH HOMELESSNESS SYSTEMATIC REVIEW

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)
library(esc)

## READ DATA

data_extraction_sheet_location <- "./review/inputs/yh_review_data_extraction_sheet.xlsx"
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
        pre1mean,
        pre1sd,
        post1mean,
        post1sd,
        pre2mean,
        pre2sd,
        post2mean,	
        post2sd,
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

binary_proportions <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Binary proportions"
  ) %>%
  mutate(
    id = row_number()
  )

## TRANSFORM RESULTS REPORTED AS MEAN GAINS TO HEDGES' G USING ESC PACKAGE

mean_gain_hedges_g <- mean_gain %>%
  # run es function
  effect_sizes(
    study = study_id,
    fun = "esc_mean_gain",
    grp1n = grp1n,
    grp2n = grp2n,
    pre1mean = pre1mean,
    pre1sd = pre1sd,
    post1mean = post1mean,
    post1sd = post1sd,
    pre2mean = pre2mean,
    pre2sd = pre2sd,
    post2mean = post2mean,
    post2sd = post2sd,
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
    sample_size,
    favourable_direction
  ) 

## TRANSFORM RESULTS REPORTED AS MEAN DIFFERENCES TO HEDGES' G USING CUSTOM FUNCTION

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported mean differences
esc_mean_difference <- function(
  mean_difference, 
  mean_diff_se = NULL, 
  mean_diff_ci_lower = NULL, 
  mean_diff_ci_upper = NULL, 
  treatment_n, 
  comparison_n, 
  esc_type) {
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

# Transform mean difference results from Kozloff 2016 et al that reports CI's
mean_difference_hedges_g_kozloff <- mean_difference %>%
  # filter results from Kozloff 2016
  filter(
    study_id == "kozloff_2016"
  ) %>%
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
  )) 

# Transform mean difference results from Gilmer 2016 that reports SE's 
mean_difference_hedges_g_gilmer <- mean_difference %>%
  # filter results from Gilmer 2016
  filter(
    study_id == "gilmer_2016"
  ) %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_mean_difference(
      mean_difference = mean_diff,
      mean_diff_se = mean_diff_se,
      treatment_n = grp1n,
      comparison_n = grp2n,
      esc_type = "g"
    )
  )) 

# Merge results back together and clean up
mean_difference_hedges_g <- bind_rows(
  mean_difference_hedges_g_kozloff,
  mean_difference_hedges_g_gilmer
  ) %>%
  ungroup() %>%
  unnest_wider(
    result
  ) %>%
  # Convert the character column to a list of numeric vectors
  mutate(
    conf_interval = as.character(conf_interval),
    conf_interval = str_extract_all(conf_interval, "-?[0-9.]+") %>% 
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
    sample_size = grp1n + grp2n,
    p_value = round(p_value, 4)
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
    p_value,
    sample_size,
    favourable_direction
  ) 

## TRANSFORM RESULTS REPORTED AS RATIOS OF ODDS RATIOS TO HEDGES' G USING CUSTOM FUNCTION

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported ratio of odds ratios
esc_diff_odds_ratio <- function(
  odds_ratio, 
  odds_ratio_ci_lower = NULL, 
  odds_ratio_ci_upper = NULL, 
  treatment_population, 
  comparison_population, 
  esc_type = 'Cohens_d') {
  
  # Convert odds ratios to log scale
  log_odds_ratio <- log(odds_ratio)
  log_odds_ratio_lower <- log(odds_ratio_ci_lower)
  log_odds_ratio_upper <- log(odds_ratio_ci_upper)
  
  # Estimate the standard error of the log odds ratio
  se_log_odds_ratio <- (log_odds_ratio_upper - log_odds_ratio_lower) / (2 * 1.96)
  
  # Calculate the log odds ratio variance
  var_log_odds_ratio <- se_log_odds_ratio^2
  
  # Assuming Poisson distribution, estimate the mean count for each group
  treatment_mean_count <- treatment_population * odds_ratio
  comparison_mean_count <- comparison_population
  
  # Calculate the pooled mean count
  pooled_mean_count <- (treatment_mean_count + comparison_mean_count) / 2
  
  # Estimate the pooled standard deviation
  pooled_sd <- sqrt(pooled_mean_count)
  
  # Convert the log odds ratio to the raw scale
  odds_ratio <- exp(log_odds_ratio)
  
  # Calculate Cohen's d
  cohens_d <- (odds_ratio - 1) * sqrt(pooled_mean_count)
  
  # Calculate the standard error for Cohen's d
  standard_error_d <- se_log_odds_ratio * sqrt(pooled_mean_count)
  
  # Calculate the confidence interval for Cohen's d (95% confidence level, Z = 1.96)
  cohens_d_ci_lower <- cohens_d - (1.96 * standard_error_d)
  cohens_d_ci_upper <- cohens_d + (1.96 * standard_error_d)
  
  # Calculate the Z-statistic
  z_stat <- log_odds_ratio / se_log_odds_ratio
  
  # Calculate the p-value using the standard normal distribution
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # Check if Hedges' g is requested
  if (esc_type == 'Hedges_g') {
    # Calculate Hedges' g
    hedges_g <- cohens_d * (1 - (3 / (4 * (treatment_population + comparison_population) - 9)))
    return(list(effect_size = hedges_g, conf_interval = c(cohens_d_ci_lower, cohens_d_ci_upper), p_value = p_value))
  } else {
    return(list(effect_size = cohens_d, conf_interval = c(cohens_d_ci_lower, cohens_d_ci_upper), p_value = p_value))
  }
}

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
    sample_size,
    favourable_direction
  ) 

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
    sample_size,
    favourable_direction
  ) 

## CLEAN UP DATA FOR EXPORT

effect_sizes <- dplyr::bind_rows(
  mean_gain_hedges_g,
  mean_difference_hedges_g,
  mean_sd_hedges_g,
  binary_proportions_hedges_g
  ) %>%
  # round quant outcomes for reporting and combine in single column
  mutate(
    es = round(es, 2),
    es_ci_low = round(es_ci_low, 2),
    es_ci_high = round(es_ci_high, 2)
  )

## ADD FLAGS FOR DISCUSSION

effect_sizes_info <- effect_sizes %>%
  mutate(
    # create variable that determines the precision of the estimate based on whether or not it crosses zero
    estimate_precision = case_when(
      es_ci_low < 0 & es_ci_high > 0 ~ "imprecise",
      TRUE ~ "precise"
    ),
    # create variable that determines the size of the effect based on Cohen's benchmarks
    es_magnitude = case_when(
      abs(es) < 0.2 ~ "very small",
      abs(es) >= 0.2 & abs(es) < 0.5 ~ "small",
      abs(es) >= 0.5 & abs(es) < 0.8 ~ "medium",
      abs(es) > 0.8 ~ "large"
    ),
    # create variable that shows if direction of effect favours the intervention or comparison
    direction_of_effect = case_when(
      es < 0 & favourable_direction == "Negative" ~ "Favours intervention",
      es > 0 & favourable_direction == "Negative" ~ "Favours comparison",
      es < 0 & favourable_direction == "Positive" ~ "Favours comparison",
      es > 0 & favourable_direction == "Positive" ~ "Favours intervention"
    )
  )
    
## EXPORT ES DATA

saveRDS(
  effect_sizes_info,
  "./review/output/tables/effect_size_results.RDS"
  )

write_csv(
  effect_sizes_info,
  "./review/output/tables/effect_size_results.csv"
  )