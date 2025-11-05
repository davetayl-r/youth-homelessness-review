# Create PRISMA 2020 figure for manuscript

# load packages within groundhog framework
library(tidyverse)
library(PRISMA2020)
library(magick)

# read data
raw_prisma_data_location <- "./review/inputs/yh_review_prisma_data.csv"
raw_prisma_data <- read.csv(raw_prisma_data_location)

# create prisma flowchart
prisma_data <- PRISMA_data(raw_prisma_data)

prisma_figure <- PRISMA_flowdiagram(
  data = prisma_data,
  detail_databases = TRUE,
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

# export flowchart as svg
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./review/output/visualisation/yh_review_prisma_flowchart.png",
  filetype = "png",
  overwrite = TRUE
)

# export flowchart as svg
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./review/output/visualisation/yh_review_prisma_flowchart.svg",
  filetype = "svg",
  overwrite = TRUE
)

# read colour image back in
original_prisma_flowchart_location <- "./review/output/visualisation/yh_review_prisma_flowchart.svg"
original_prisma_flowchart <- image_read_svg(original_prisma_flowchart_location)

# convert to grayscale
grayscale_prisma_flowchart <- image_modulate(
  original_prisma_flowchart,
  saturation = 0
)

# increase brightness
grayscale_prisma_flowchart_light <- image_modulate(
  grayscale_prisma_flowchart,
  brightness = 107
)
grayscale_prisma_flowchart_light

# export grayscale image
image_write(
  grayscale_prisma_flowchart_light,
  path = "./review/output/visualisation/yh_review_prisma_flowchart_grayscale.png",
  quality = 100
)
