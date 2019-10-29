# Cleans the raw files before moving them to data
# Arseniy Khvorov
# Created 2019/10/24
# Last edit 2019/10/24

library(readr)
suppressPackageStartupMessages(library(dplyr))
library(purrr)

data_folder <- "data"
data_raw_folder <- "data-raw"

read_raw_csv <- function(name, folder, col_types = cols()) {
  read_csv(
    file.path(folder, paste0(name, ".csv")), col_types = col_types
  ) %>% mutate(filename = name)
}

save_csv <- function(data, name, folder) {
  write_csv(data, file.path(folder, paste0(name, ".csv")))
}

alex_files <- c("H273", "N197")

alex_data <- map_dfr(alex_files, read_raw_csv, data_raw_folder) %>%
  select(
    recipient = "% in Recipient", donor = "% in Donor", mutation = filename
  ) %>%
  mutate(recipient = recipient / 100, donor = donor / 100) %>%
  group_split(mutation)
names(alex_data) <- map_chr(alex_data, function(tbl) unique(tbl$mutation))

iwalk(alex_data, save_csv, data_folder)
