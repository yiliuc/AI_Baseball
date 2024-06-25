library(dplyr)
data_2023 <- read.csv("inputs/data/all2023.csv", header=FALSE)

description_file <- here("documents", "descriptions.txt")
description_df <- read.delim(description_file, stringsAsFactors = FALSE)

mapping <- data.frame(
  original = paste0("V", 1:nrow(description_df)),
  new = description_df$Header
)

colnames(data_2023) <- mapping$new[match(colnames(data_2023), mapping$original)]

write.csv(data_2023, "Data/all2023_updated.csv", row.names = FALSE)

# Read the new csv
# data_2023_updated <- read.csv("Data/all2023_updated.csv")