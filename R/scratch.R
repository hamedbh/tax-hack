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
    summarise_at(vars(weighted_TI, FACT), sum, na.rm = TRUE) %>% 
    mutate(income_pp = weighted_TI/FACT) %>% 
    #summarise(income = sum(as.double(weighted_TI), na.rm = TRUE)) %>% 
    ggplot(aes(x = year, 
               y = income_pp, 
               colour = gender)) + 
    geom_point() + 
    geom_smooth(method = "gam") + 
    scale_colour_manual(values = c("black", "orange")) + 
    theme_classic() + 
    NULL

gender_df %>% 
    group_by(year, gender) %>% 
    summarise_at(vars(weighted_TI, FACT), median, na.rm = TRUE) %>% 
    mutate(income_pp = weighted_TI/FACT) %>% 
    #summarise(income = sum(as.double(weighted_TI), na.rm = TRUE)) %>% 
    ggplot(aes(x = year, 
               y = income_pp, 
               colour = gender)) + 
    geom_point() + 
    geom_smooth(method = "gam") + 
    scale_colour_manual(values = c("black", "orange")) + 
    theme_classic() + 
    NULL

# generate the side by side median pay barplot
gender_df %>% 
    group_by(year, gender) %>% 
    summarise_at(vars(weighted_TI, FACT), median, na.rm = TRUE) %>% 
    mutate(income_pp = weighted_TI/FACT) %>% 
    ggplot(aes(x = year, 
               y = income_pp, 
               fill = gender)) + 
    geom_col(position = "dodge") + 
    scale_fill_manual(values = c("black", "orange")) + 
    theme_classic() + 
    labs(x = "Year", y = "Median Income", fill = "Gender") + 
    NULL

grouped_gender_df <- gender_df %>% 
    group_by(year, gender) %>% 
    summarise_at(vars(weighted_TI, FACT), median, na.rm = TRUE) %>% 
    mutate(income_pp = weighted_TI/FACT) %>% 
    ungroup()

gender_gap_df <- grouped_gender_df %>% 
    filter(gender == "Male") %>% 
    transmute(year, male_income_pp = income_pp) %>% 
    inner_join(grouped_gender_df %>% 
                   filter(gender == "Female") %>% 
                   transmute(year, female_income_pp = income_pp), 
               by = "year") %>% 
    mutate(gender_gap = male_income_pp - female_income_pp)

gender_gap_df %>% 
    ggplot(aes(x = year, 
               y = gender_gap)) + 
    geom_point(colour = "darkgreen") + 
    geom_smooth(method = "gam", colour = "red") + 
    theme_classic() + 
    NULL
gender_df %>% 
    group_by(year, gender) %>% 
    summarise_at(vars(weighted_TI, FACT), median, na.rm = TRUE) %>% 
    mutate(income_pp = weighted_TI/FACT) %>% 
    unite(col = "year_gender", year, gender)
    spread(key = vars(year, gender), value = income_pp)
