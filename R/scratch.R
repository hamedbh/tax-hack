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

table(as.factor(raw_df$INDUSTRY07))
gender_df <- raw_df %>% 
    filter(str_detect(INDUSTRY07, pattern = "\\d{4}|[A-U]")) %>% 
    transmute(year = as.integer((substr(year, 1, 4))), 
              gender = if_else(SEX == 1L, 
                               "Male", 
                               "Female") %>% 
                  as_factor(), 
              FACT, 
              TI, 
              weighted_TI = FACT * TI, 
              INDUSTRY07)
gender_df %>% 
    #sample_n(size = 1000, weight = FACT) %>% 
    group_by(year) %>% 
    summarise(income = sum(as.double(TI), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, 
               y = income)) + 
    geom_point() + 
    geom_smooth(method = "gam")

