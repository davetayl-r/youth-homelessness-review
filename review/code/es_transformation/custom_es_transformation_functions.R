## CUSTOM ES TRANSFORMATION FUNCTIONS

# Purpose: Most common transformations can be handled using those in the esc() R package. 
# However, the results in the study by Kozloff (2016) are presented as treatment effects (i.e., mean differences, ratios or rate ratios and ratios of odds ratios and their confidence intervals).
# These three functions transform these results into a common measure of the standardised mean difference: Cohen's d or Hedge's g

## Transform ratio of odds ratios

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported ratio of odds ratios and baseline rates
esc_ratio_odds_ratio <- function(
  ratio_odds_ratio,
  ratio_odds_ratio_se = NULL,
  ratio_odds_ratio_ci_lower = NULL,
  ratio_odds_ratio_ci_upper = NULL,
  treatment_n,
  comparison_n,
  population_baseline_rate,
  esc_type = 'd') {
  
  # Use reported standard error if provided, otherwise calculate standard error from confidence intervals
  if (!is.null(ratio_odds_ratio_se)) {
    se_log_ratio_odds_ratio <- ratio_odds_ratio_se / ratio_odds_ratio
  } else if (!is.null(ratio_odds_ratio_ci_lower) & !is.null(ratio_odds_ratio_ci_upper)) {
    log_ratio_odds_ratio_ci_lower <- log(ratio_odds_ratio_ci_lower)
    log_ratio_odds_ratio_ci_upper <- log(ratio_odds_ratio_ci_upper)
    se_log_ratio_odds_ratio <- (log_ratio_odds_ratio_ci_upper - log_ratio_odds_ratio_ci_lower) / (2 * 1.96)
  } else {
    stop("Either ratio_odds_ratio_se or both ratio_odds_ratio_ci_lower and ratio_odds_ratio_ci_upper must be provided.")
  }
  
  # Convert ratio of odds ratios to log scale
  log_ratio_odds_ratio <- log(ratio_odds_ratio)
  
  # Estimate the event probability in the comparison group using the population baseline rate
  comparison_event_prob <- population_baseline_rate
  
  # Estimate the odds in the comparison group
  comparison_odds <- comparison_event_prob / (1 - comparison_event_prob)
  
  # Estimate the odds in the treatment group using the ratio of odds ratios and the comparison odds
  treatment_odds <- comparison_odds * ratio_odds_ratio
  
  # Convert odds to event probabilities
  treatment_event_prob <- treatment_odds / (1 + treatment_odds)
  
  # Convert event probabilities to approximate event counts
  treatment_event_count <- treatment_n * treatment_event_prob
  comparison_event_count <- comparison_n * comparison_event_prob
  
  # Calculate the pooled event count
  pooled_event_count <- (treatment_event_count + comparison_event_count) / 2
  
  # Estimate the pooled event rate
  pooled_event_rate <- pooled_event_count / ((treatment_n + comparison_n) / 2)
  
  # Estimate the pooled standard deviation using the pooled event rate
  pooled_sd <- sqrt(pooled_event_rate * (1 - pooled_event_rate))
  
  # Calculate Cohen's d
  cohens_d <- (treatment_event_prob - comparison_event_prob) / pooled_sd
  
  # Calculate the standard error for Cohen's d using the event probabilities and sample sizes
  standard_error_d <- sqrt((treatment_event_prob * (1 - treatment_event_prob)) / treatment_n + (comparison_event_prob * (1 - comparison_event_prob)) / comparison_n) / pooled_sd
  
  # Calculate the confidence interval for Cohen's d
  cohens_d_ci_lower <- cohens_d - (1.96 * standard_error_d)
  cohens_d_ci_upper <- cohens_d + (1.96 * standard_error_d)
  
  # Calculate the Z-statistic
  z_stat <- log_ratio_odds_ratio / se_log_ratio_odds_ratio
  
  # Calculate the p-value using the standard normal distribution
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # Check if Hedges' g is requested
  if (esc_type == 'g') {
    # Calculate Hedges' g
    hedges_g <- cohens_d * (1 - (3 / (4 * (treatment_n + comparison_n) - 9)))
    return(list(effect_size = hedges_g, conf_interval = c(cohens_d_ci_lower, cohens_d_ci_upper), p_value = p_value))
  } else {
    return(list(effect_size = cohens_d, conf_interval = c(cohens_d_ci_lower, cohens_d_ci_upper), p_value = p_value))
  }
}

## Transform mean differences

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
## Transform ratio of rate ratios

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported differences in rate ratios and baseline rates
esc_ratio_rate_ratio <- function(
  ratio_rate_ratio,
  ratio_rate_ratio_se = NULL,
  ratio_rate_ratio_ci_lower = NULL,
  ratio_rate_ratio_ci_upper = NULL,
  treatment_n,
  comparison_n,
  population_baseline_rate,
  esc_type = 'd') {
  
  # Use reported standard error if provided, otherwise calculate standard error from confidence intervals
  if (!is.null(ratio_rate_ratio_se)) {
    se_ratio_rate_ratio <- ratio_rate_ratio_se
  } else if (!is.null(ratio_rate_ratio_ci_lower) & !is.null(ratio_rate_ratio_ci_upper)) {
    se_ratio_rate_ratio <- (ratio_rate_ratio_ci_upper - ratio_rate_ratio_ci_lower) / (2 * 1.96)
  } else {
    stop("Either ratio_rate_ratio_se or both ratio_rate_ratio_ci_lower and ratio_rate_ratio_ci_upper must be provided.")
  }
  
  # Estimate the event count for the comparison group using the population baseline rate
  comparison_event_count <- comparison_n * population_baseline_rate
  
  # Estimate the event count for the treatment group using the rate ratio and the comparison event count
  treatment_event_count <- comparison_event_count * ratio_rate_ratio
  
  # Calculate the pooled event count
  pooled_event_count <- (treatment_event_count + comparison_event_count) / 2
  
  # Estimate the pooled event rate
  pooled_event_rate <- pooled_event_count / ((treatment_n + comparison_n) / 2)
  
  # Estimate the pooled standard deviation using the pooled event rate
  pooled_sd <- sqrt(pooled_event_rate * (1 - pooled_event_rate))
  
  # Calculate Cohen's d
  cohens_d <- (treatment_event_count / treatment_n - comparison_event_count / comparison_n) / pooled_sd
  
  # Calculate the standard error for Cohen's d using the event rates and sample sizes
  standard_error_d <- sqrt((treatment_event_count / treatment_n^2) + (comparison_event_count / comparison_n^2)) / pooled_sd
  
  # Calculate the confidence interval for Cohen's d
  cohens_d_ci_lower <- cohens_d - (1.96 * standard_error_d)
  cohens_d_ci_upper <- cohens_d + (1.96 * standard_error_d)
  
  # Calculate the Z-statistic
  z_stat <- (ratio_rate_ratio - 1) / se_ratio_rate_ratio
  
  # Calculate the p-value using the standard normal distribution
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  # Check if Hedges' g is requested
  if (esc_type == 'g') {
    # Calculate Hedges' g
    hedges_g <- cohens_d * (1 - (3 / (4 * (treatment_n + comparison_n) - 9)))
    return(list(effect_size = hedges_g, conf_interval = c(hedges_g - (1.96 * standard_error_d), hedges_g + (1.96 * standard_error_d)), p_value = p_value))
  } else {
    return(list(effect_size = cohens_d, conf_interval = c(cohens_d_ci_lower, cohens_d_ci_upper), p_value = p_value))
  }
}
 
# testing sandpit 
diff <- 0.67
diff_lower <- 0.22
diff_upper <- 2.07
treatment_n <- 87
comparison_n <- 69
population_baseline_rate <- 0.2

esc_mean_difference(
  mean_difference = diff, 
  mean_diff_ci_lower = diff_lower, 
  mean_diff_ci_upper = diff_upper, 
  treatment_n = treatment_n, 
  comparison_n = comparison_n, 
  esc_type = "g")

esc_ratio_rate_ratio(
  ratio_rate_ratio = diff,
  ratio_rate_ratio_ci_lower = diff_lower,
  ratio_rate_ratio_ci_upper = diff_upper,
  population_baseline_rate = population_baseline_rate,
  treatment_n = treatment_n,
  comparison_n = comparison_n,
  esc_type = "g")

esc_ratio_odds_ratio(
  ratio_odds_ratio = diff,
  ratio_odds_ratio_ci_lower = diff_lower,
  ratio_odds_ratio_ci_upper = diff_upper,
  population_baseline_rate = population_baseline_rate,
  treatment_n = treatment_n,
  comparison_n = comparison_n,
  esc_type = "g")
