## PRISMA CALCULATIONS FOR YOUTH HOMELESSNESS REVIEW

## LOAD REQUIRED PACKAGES

library(tidyverse)

## LOAD SEARCH RIS FILES

ris_files_location <- "./review/inputs/raw_database_results"

ris_files <- list.files(
  ris_files_location, 
  pattern = "\\.ris$", 
  ignore.case = TRUE,
  full.names = TRUE)

# Function to count citations in a RIS file
count_citations <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  count <- sum(grepl("^TY", lines))
  return(data.frame(file = basename(file_path), count = count))
}

# Call function
citation_counts <- lapply(ris_files, count_citations)

# Combine into a single dataframe
citation_counts_df <- bind_rows(citation_counts)

# View the results
print(citation_counts_df)

# Check total aligns with total imported into Endnote (n=4332)
sum(citation_counts_df$count)
