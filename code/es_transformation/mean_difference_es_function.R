## FUNCTION TO CALCULATE SMD FROM A REPORTED MEAN DIFFERENCE AND 95 PER CENT CONFIDENCE INTERVAL

# The results in the study by Kozloff (2016) are presented as treatment effects (i.e., mean differences and confidence intervals).
# This function transforms these into a common measure of the standardised mean difference: Cohen's d or Hedge's g

# Function to estimate either Cohen's d or Hedges' g, along with their 95% confidence interval and derived p-value
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

# Example usage for Cohen's d using confidence intervals
result_d_ci <- esc_mean_difference(
  mean_difference = 23.39, 
  treatment_n = 87, 
  comparison_n = 69, 
  esc_type = 'd', 
  mean_diff_ci_lower = 10.28, 
  mean_diff_ci_upper = 36.5)

# Example usage for Cohen's d using reported SE

mean_diff_ci_lower <- 10.28
mean_diff_ci_upper <- 36.5
mean_diff_se <- (mean_diff_ci_upper - mean_diff_ci_lower) / (2 * 1.96)

result_d_se <- esc_mean_difference(
  mean_difference = 23.39, 
  treatment_n = 87, 
  comparison_n = 69, 
  esc_type = 'd', 
  mean_diff_se = mean_diff_se)

