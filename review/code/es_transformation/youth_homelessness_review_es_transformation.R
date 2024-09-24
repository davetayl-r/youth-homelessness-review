## Housing and support interventions for homeless youth systematic review
## Effect Size transformations

# load packages
library(tidyverse)
library(readxl)
library(esc)

# load custom es transformation functions
source("./review/code/es_transformation/custom_es_transformation_functions.R")

# read raw data
data_extraction_sheet_location <- "./review/inputs/yh_review_data_extraction_sheet.xlsx"
data_extraction_sheet <- read_excel(
  path = data_extraction_sheet_location,
  sheet = "outcome_data",
  col_names = TRUE
)

# clean data
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
        diff,
        diff_lower,
        diff_upper,
        diff_se,
        population_baseline_rate
        ),
      as.numeric
    )
  ) %>%
  # exclude baseline measurements
  filter(
    !outcome_timing == 0
  )

# group data by effect size type
mean_gain <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean Gain"
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

mean_difference <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Mean Difference"
  ) %>%
  mutate(
    id = row_number()
  )

ratio_rate_ratio <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Ratio of Rate Ratios"
  ) %>%
  mutate(
    id = row_number()
  )

ratio_odds_ratio <- clean_data_extraction_sheet %>%
  filter(
    esc_type == "Ratio of Odds Ratios"
  ) %>%
  mutate(
    id = row_number()
  )

# transform results reported as mean and standard deviation gains to hedges' g using esc package
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

# transform results reported as binary proportions to hedges' g using esc package
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

# transform results reported as mean differences from Kozloff 2016 et al that reports CI's using custom function
mean_difference_hedges_g_kozloff_2016 <- mean_difference %>%
  # filter results from Gilmer 2016
  filter(
    study_id == "kozloff_2016"
  ) %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_mean_difference(
      mean_difference = diff,
      mean_diff_ci_lower = diff_lower,
      mean_diff_ci_upper = diff_upper,
      treatment_n = grp1n,
      comparison_n = grp2n,
      esc_type = "g"
    )
  )) 

# transform results reported as mean differences from Gilmer 2016 that reports SE's using custom function
mean_difference_hedges_g_gilmer_2016 <- mean_difference %>%
  # filter results from Gilmer 2016
  filter(
    study_id == "gilmer_2016"
  ) %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_mean_difference(
      mean_difference = diff,
      mean_diff_se = diff_se,
      treatment_n = grp1n,
      comparison_n = grp2n,
      esc_type = "g"
    )
  )) 

# merge results back together and clean up
mean_difference_hedges_g <- bind_rows(
  mean_difference_hedges_g_kozloff_2016,
  mean_difference_hedges_g_gilmer_2016
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
  # split the list-column into two new columns
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

# transform results reported as ratios of rate ratios using custom function
ratio_rate_ratio_hedges_g <- ratio_rate_ratio %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_ratio_rate_ratio(
      ratio_rate_ratio = diff, 
      ratio_rate_ratio_ci_lower = diff_lower, 
      ratio_rate_ratio_ci_upper = diff_upper, 
      population_baseline_rate = population_baseline_rate,
      treatment_n = grp1n, 
      comparison_n = grp2n, 
      esc_type = "g"
    )
  )) %>%
  # clean up results
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
  # split the list-column into two new columns
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

# transform results reported as ratios of odds ratios using custom function
ratio_odds_ratio_hedges_g <- ratio_odds_ratio %>%
  # run custom function
  rowwise() %>%
  mutate(result = list(
    esc_ratio_odds_ratio(
      ratio_odds_ratio = diff, 
      ratio_odds_ratio_ci_lower = diff_lower, 
      ratio_odds_ratio_ci_upper = diff_upper, 
      population_baseline_rate = population_baseline_rate,
      treatment_n = grp1n, 
      comparison_n = grp2n, 
      esc_type = "g"
    )
  )) %>%
  # clean up results
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
  # split the list-column into two new columns
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

# clean up data for export 
effect_sizes <- dplyr::bind_rows(
  mean_sd_hedges_g,
  binary_proportions_hedges_g,
  mean_difference_hedges_g,
  ratio_rate_ratio_hedges_g,
  ratio_odds_ratio_hedges_g
  ) %>%
  # round quant outcomes for reporting and combine in single column
  mutate(
    es = round(es, 2),
    es_ci_low = round(es_ci_low, 2),
    es_ci_high = round(es_ci_high, 2)
  )

# add flags for discussion
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
  ) %>%
  # drop redundant vars
  select(
    -p_value
  )
    
# export effect size data
saveRDS(
  effect_sizes_info,
  "./review/output/tables/effect_size_results.RDS"
  )

write_csv(
  effect_sizes_info,
  "./review/output/tables/effect_size_results.csv"
  )

housing <- effect_sizes_info %>% filter(outcome_domain == "Stable Housing")
health <- effect_sizes_info %>% filter(outcome_domain == "Health")