library(data.table)
library(tidyverse)
library(here)
raw_df <- rbindlist(lapply(list.files(here("data/raw"), 
                                      full.names = TRUE, 
                                      pattern = "tab$"), 
                           function(path) {
                               tmp <- fread(path, fill = TRUE)
                               tmp[, year := str_extract(path, 
                                                         pattern = "\\d{4}-\\d{2}")]
                           }), 
                    fill = TRUE) %>% 
    as_tibble()
