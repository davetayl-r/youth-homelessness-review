## RISK OF BIAS PLOTS: ROB2 

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(robvis)
library(cowplot)

## READ DATA

raw_rob_location <- "./inputs/yh_review_risk_of_bias_data.xlsx"

raw_robins_i_data <- read_excel(
  raw_rob_location,
  skip = 1,
  sheet = "robins-i")

raw_rob2_data <- read_excel(
  raw_rob_location,
  skip = 1,
  sheet = "rob2")

## CLEAN DATA

robins_i_data <- raw_robins_i_data %>%
  # remove instructions
  slice(
    -1
  ) %>%
  # select summary measures for each domain
  select(
    study_id,
    starts_with("Risk-of-bias judgement"),
    `Overall ROB`
  ) %>%
  # rename vars
  rename(
    Label = study_id,
    D1 = `Risk-of-bias judgement...22`,
    D2 = `Risk-of-bias judgement...33`,
    D3 = `Risk-of-bias judgement...40`,
    D4 = `Risk-of-bias judgement...55`,
    D5 = `Risk-of-bias judgement...66`, 
    D6 = `Risk-of-bias judgement...75`,
    D7 = `Risk-of-bias judgement...82`,
    Overall = `Overall ROB`
  ) %>%
  # rename study reference
  mutate(
    Label = case_when(
      Label == "raithel_2015" ~ "Raithel et al 2015",
      Label == "gilmer_2016" ~ "Gilmer et al 2016"
      ),
    # wrap label if too long
    Label = str_wrap(Label, width = 25)
  ) %>%
  # drop duplicates
  distinct()


rob2_data <- raw_rob2_data %>%
  # remove instructions
  slice(
    -1
  ) %>%
  # select summary measures for each domain
  select(
    study_id,
    starts_with("Risk-of-bias judgement"),
    -`Risk-of-bias judgement...42`,
    `Overall ROB`
  ) %>%
  # rename vars
  rename(
    Label = study_id,
    D1 = "Risk-of-bias judgement...12",
    D2 = "Risk-of-bias judgement...28",
    D3 = "Risk-of-bias judgement...51",
    D4 = "Risk-of-bias judgement...62",
    D5 = "Risk-of-bias judgement...69", 
    Overall = "Overall ROB"
  ) %>%
  # wrap label if too long
  mutate(
    Label = case_when(
      Label == "kidd_2020" ~ "Kidd et al 2020",
      Label == "kozloff_2016" ~ "Kozloff et al 2016",
      Label == "thulien_2022" ~ "Thulien et al 2022",
      Label == "slesnick_2023" ~ "Slesnick et al 2023"   
    ),
    Label = str_wrap(Label, width = 25)
  ) %>%
  # drop duplicates
  distinct()

## CREATE SUMMARY PLOTS

robins_i_summary_plot <- rob_summary(
  data = robins_i_data, 
  tool = "ROBINS-I",
  overall = TRUE,
  weighted = FALSE,
  colour = c(
    "#008744",
    "#ffa700",
    "#f37735",
    "#d62d20")) +
  scale_x_discrete(labels = wrap_format(25)) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(colour = "#FFFFFF"),
    legend.margin = margin(1,0,1,0)
  )

rob2_summary_plot <- rob_summary(
  data = rob2_data, 
  tool = "ROB2",
  overall = TRUE,
  weighted = FALSE,
  colour = c(
    "#008744",
    "#ffa700",
    "#d62d20")) +
  scale_x_discrete(labels = wrap_format(25)) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(colour = "#FFFFFF"),
    legend.margin = margin(1,0,1,0)
  ) 
    
## CREATE TRAFFIC LIGHT PLOT

rob2_traffic_light_plot <- rob_traffic_light(
  data = rob2_data, 
  tool = "ROB2",
  psize = 12,
  colour = c(
    "#008744",
    "#ffa700",
    "#d62d20")) +
  theme(
    plot.margin = margin(20,5,0,5),
    strip.text.y.left = element_text(angle = 0)
  )
  
robins_i_traffic_light_plot <- rob_traffic_light(
  data = robins_i_data, 
  tool = "ROBINS-I",
  psize = 12,
  colour = c(
    "#008744",
    "#ffa700",
    "#f37735",
    "#d62d20")) +
  theme(
    plot.margin = margin(20,5,0,5),
    strip.text.y.left = element_text(angle = 0)
  )

## COMBINE PLOTS FOR PUBLICATION

summary_rob_plots <- plot_grid(
  rob2_summary_plot, 
  robins_i_summary_plot, 
  labels = c(
    'RoB2 — Randomised studies (n=4)', 
    'ROBINS-I — Non-randomised studies (n=2)'),
  label_size = 12,
  ncol = 2,
  hjust = -0.1, 
  align = "h",
  axis = "bt")

traffic_light_plots <- plot_grid(
  rob2_traffic_light_plot, 
  robins_i_traffic_light_plot, 
  labels = c(
    'RoB2 — Randomised studies (n=4)', 
    'ROBINS-I — Non-randomised studies (n=2)'),
  label_size = 12,
  ncol = 2,
  hjust = -0.1, 
  align = "h",
  axis = "bt")

## EXPORT PLOTS

ggsave(
  file = "./output/visualisation/yh_review_summary_rob_plot.png",
  plot = summary_rob_plots,
  height = 6,
  width = 14,
  type = "cairo-png")

ggsave(
  file = "./output/visualisation/yh_review_traffic_light_plot.png",
  plot = traffic_light_plots,
  height = 6,
  width = 14,
  type = "cairo-png")
