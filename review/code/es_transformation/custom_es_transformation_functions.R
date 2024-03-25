## CUSTOM ES TRANSFORMATION FUNCTIONS

# Purpose: Most common transformations can be handled using those in the esc() R package. 
# However, the results in the study by Kozloff (2016) are presented as treatment effects (i.e., mean differences, ratios or rate ratios and ratios of odds ratios and their confidence intervals).
# These three functions transform these results into a common measure of the standardised mean difference: Cohen's d or Hedge's g

## Transform ratio of odds ratios

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported ratio of odds ratios
esc_difference_odds_ratio <- function(
  odds_ratio,
  odds_ratio_se = NULL,
  odds_ratio_ci_lower = NULL,
  odds_ratio_ci_upper = NULL,
  treatment_n,
  comparison_n,
  esc_type = 'd') {
  
  # Use reported standard error if provided, otherwise calculate standard error from confidence intervals
  if (!is.null(odds_ratio_se)) {
    se_log_odds_ratio <- odds_ratio_se / odds_ratio
  } else if (!is.null(odds_ratio_ci_lower) & !is.null(odds_ratio_ci_upper)) {
    log_odds_ratio_ci_lower <- log(odds_ratio_ci_lower)
    log_odds_ratio_ci_upper <- log(odds_ratio_ci_upper)
    se_log_odds_ratio <- (log_odds_ratio_ci_upper - log_odds_ratio_ci_lower) / (2 * 1.96)
  } else {
    stop("Either odds_ratio_se or both odds_ratio_ci_lower and odds_ratio_ci_upper must be provided.")
  }
  
  # Convert odds ratio to log scale
  log_odds_ratio <- log(odds_ratio)
  
  # Calculate the log odds ratio variance
  var_log_odds_ratio <- se_log_odds_ratio^2
  
  # Assuming Poisson distribution, estimate the mean count for each group
  treatment_mean_count <- treatment_n * odds_ratio
  comparison_mean_count <- comparison_n
  
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

# Declare function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value from reported differences in rate ratios
esc_difference_rate_ratio <- function(
  difference_rate_ratio,
  difference_rate_ratio_se = NULL,
  difference_rate_ratio_ci_lower = NULL,
  difference_rate_ratio_ci_upper = NULL,
  treatment_n,
  comparison_n,
  esc_type = 'd') {
  
  # Use reported standard error if provided, otherwise calculate standard error from confidence intervals
  if (!is.null(difference_rate_ratio_se)) {
    se_difference_rate_ratio <- difference_rate_ratio_se
  } else if (!is.null(difference_rate_ratio_ci_lower) & !is.null(difference_rate_ratio_ci_upper)) {
    se_difference_rate_ratio <- (difference_rate_ratio_ci_upper - difference_rate_ratio_ci_lower) / (2 * 1.96)
  } else {
    stop("Either difference_rate_ratio_se or both difference_rate_ratio_ci_lower and difference_rate_ratio_ci_upper must be provided.")
  }
  
  # Estimate the mean count for each group (assuming Poisson distribution)
  treatment_mean_count <- treatment_n * difference_rate_ratio
  comparison_mean_count <- comparison_n
  
  # Calculate the pooled mean count
  pooled_mean_count <- (treatment_mean_count + comparison_mean_count) / 2
  
  # Estimate the pooled standard deviation
  pooled_sd <- sqrt(pooled_mean_count)
  
  # Calculate Cohen's d
  cohens_d <- (difference_rate_ratio - 1) * sqrt(pooled_mean_count)
  
  # Calculate the standard error for Cohen's d
  standard_error_d <- se_difference_rate_ratio * sqrt(pooled_mean_count)
  
  # Calculate the confidence interval for Cohen's d
  cohens_d_ci_lower <- cohens_d - (1.96 * standard_error_d)
  cohens_d_ci_upper <- cohens_d + (1.96 * standard_error_d)
  
  # Calculate the Z-statistic
  z_stat <- (difference_rate_ratio - 1) / se_difference_rate_ratio
  
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
