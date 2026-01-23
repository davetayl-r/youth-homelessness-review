# Create PRISMA 2020 figure for manuscript

# load packages
library(tidyverse)
library(PRISMA2020)

# read data
raw_prisma_data_location <- "./review/inputs/yh_review_prisma_data.csv"
raw_prisma_data <- read.csv(raw_prisma_data_location)

# get PRISMA_flowdiagram into an object to edit colours
function_text <- paste(deparse(PRISMA2020::PRISMA_flowdiagram), collapse = "\n")

# replace colour
function_text_bw <- gsub(
  "LightSteelBlue2",
  "grey95",
  function_text,
  fixed = TRUE
)

# recreate function
PRISMA_flowdiagram_custom <- eval(parse(
  text = sprintf("(%s)", function_text_bw)
))

# create prisma flowchart
prisma_data <- PRISMA_data(raw_prisma_data)

# run custom function
prisma_figure <- PRISMA_flowdiagram_custom(
  data = prisma_data,
  detail_databases = TRUE,
  interactive = FALSE,
  previous = FALSE,
  other = TRUE,
  fontsize = 12,
  font = "Helvetica",
  title_colour = "grey75",
  greybox_colour = "Gainsboro",
  main_colour = "Black",
  arrow_colour = "Black",
  arrow_head = "normal",
  arrow_tail = "none",
  side_boxes = TRUE
)

# inspect results
prisma_figure

# export flowchart as png
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./review/output/visualisation/yh_review_prisma_flowchart.png",
  filetype = "png",
  overwrite = TRUE
)

# export flowchart as svg
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./review/output/visualisation/yh_review_prisma_flowchart.pdf",
  filetype = "pdf",
  overwrite = TRUE
)

# export flowchart as svg
PRISMA_save(
  plotobj = prisma_figure,
  filename = "./review/output/visualisation/yh_review_prisma_flowchart.svg",
  filetype = "svg",
  overwrite = TRUE
)
