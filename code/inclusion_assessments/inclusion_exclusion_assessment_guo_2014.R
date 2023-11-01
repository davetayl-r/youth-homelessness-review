## STUDY ELIGIBILITY INVESTIGATION

# Issue: the study population in Guo et al (2014) is partially within the eligibility range. This is a quick check to see the proportion
# that is likely to be within range

## LOAD REQUIRED PACKAGES 

library(tidyverse)
library(ggplot2)

# Assume sample is normally distributed, simulate data with the mean and standard deviation from the paper
set.seed(123)  # for reproducibility

n <- 60
study_mean_age <- 26.3
study_sd_age <- 6.01
simulated_distribution_ages <- rnorm(n, study_mean_age, study_sd_age)

# Create a data frame for visualization
simulated_study_population_data <- data.frame(Age = simulated_distribution_ages) |>
  mutate(
    Age = round(Age)
  )

# Calculate the proportion of ages < 25
proportion_less_than_25 <- mean(simulated_distribution_ages <= 25)

# Create a histogram with bins

guo_2014_test_distribution <- simulated_study_population_data |>
  ggplot( 
    aes(
      x = Age, 
      fill = Age <= 25)
  ) +
  geom_histogram(
    binwidth = 1
  ) +
  labs(
    title = "Simulated Age Distribution from provided Mean & SD",
    subtitle = "Assumes data is normally distributed",
    x = "Age",
    y = "Frequency",
    fill = "Age less than 25"
  ) +
  scale_fill_manual(values = c("blue", "red")) +
  annotate(
    "text", 
    x = 25, 
    y = 6, 
    label = sprintf("Proportion < 25: %.2f", proportion_less_than_25), size = 4, color = "black") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    legend.position = "bottom"
  )

## EXPORT

ggsave(
  "./visualisation/inclusion_decisions/guo_population_distribution_simulation.png",
  guo_2014_test_distribution
  )