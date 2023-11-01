## RISK OF BIAS PLOTS: ROB2 

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(robvis)
library(cowplot)

## READ DATA

raw_rob_location <- "./figures/plot_data/risk_of_bias_data.xlsx"

raw_robins_i_data <- read_excel(
  raw_rob_location,
  sheet = "ROBINS-I")

raw_rob2_data <- read_excel(
  raw_rob_location,
  sheet = "ROB2")

## CLEAN DATA

robins_i_data <- raw_robins_i_data %>%
  mutate(Label = paste(Study, Reference, sep = " ")) %>%
  select(
    Label,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    Overall
  ) %>%
  mutate(
    Label = str_wrap(Label, width = 25)
  )

rob2_data <- raw_rob2_data %>%
  mutate(Label = paste(Study, Reference, sep = " ")) %>%
  select(
    Label,
    D1,
    D2,
    D3,
    D4,
    D5,
    Overall
  ) %>%
  mutate(
    Label = str_wrap(Label, width = 25)
  )

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
    'RoB2 — Randomised studies (n=8)', 
    'ROBINS-I — Non-randomised studies (n=10)'),
  label_size = 12,
  ncol = 2,
  hjust = -0.1, 
  align = "h",
  axis = "bt")

traffic_light_plots <- plot_grid(
  rob2_traffic_light_plot, 
  robins_i_traffic_light_plot, 
  labels = c(
    'RoB2 — Randomised studies (n=8)', 
    'ROBINS-I — Non-randomised studies (n=10)'),
  label_size = 12,
  ncol = 2,
  hjust = -0.1, 
  align = "h",
  axis = "bt")

## EXPORT PLOTS

ggsave(
  file = "./figures/output/transitions_review_summary_rob_plot.png",
  plot = summary_rob_plots,
  height = 6,
  width = 14,
  type = "cairo-png")

ggsave(
  file = "./figures/output/transitions_review_traffic_light_plot.png",
  plot = traffic_light_plots,
  height = 10.5,
  width = 14,
  type = "cairo-png")
