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
raw_df %>% 
    mutate(good_ind = str_detect(INDUSTRY07, pattern = "\\d{4}|[A-U]")) %>% 
    group_by(year) %>% 
    summarise(has_good_ind = sum(good_ind, na.rm = TRUE)) %>% 
    ungroup()
gender_df <- raw_df %>% 
    #filter(str_detect(INDUSTRY07, pattern = "\\d{4}|[A-U]")) %>% 
    transmute(year = as.integer((substr(year, 1, 4))), 
              gender = if_else(SEX == 1L, 
                               "Male", 
                               "Female") %>% 
                  as_factor(), 
              FACT, 
              TI, 
              weighted_TI = FACT * TI, 
              INDUSTRY07)
raw_df %>% 
    group
gender_df %>% 
    group_by(year, gender) %>% 
    summarise(income = sum(weighted_TI, na.rm = TRUE)) %>% 
    # mutate(income = scales::comma(income))
    # ungroup() %>% 
    ggplot(aes(x = year, 
               y = income, 
               colour = gender)) + 
    geom_point() + 
    geom_smooth(method = "gam") + 
    NULL
    # facet_wrap(~ gender)

