## PRISMA CHART

library(tidyverse)
library(readxl)
library(PRISMA2020)

## READ DATA

raw_prisma_data_location <- "./figures/plot_data/prisma_data.csv"
raw_prisma_data <- read.csv(raw_prisma_data_location)

## CREATE PRISMA CHART

prisma_data <- PRISMA_data(raw_prisma_data)

prisma_figure <- PRISMA_flowdiagram(
  data = prisma_data,
  interactive = FALSE,
  previous = FALSE,
  other = TRUE,
  fontsize = 12,
  font = "Helvetica",
  title_colour = "Goldenrod1",
  greybox_colour = "Gainsboro",
  main_colour = "Black",
  arrow_colour = "Black",
  arrow_head = "normal",
  arrow_tail = "none",
  side_boxes = TRUE
)

PRISMA_save(
  plotobj = prisma_figure, 
  filename = "./figures/output/transitions_review_prisma_flowchart.png",
  filetype = "png",
  overwrite = TRUE)