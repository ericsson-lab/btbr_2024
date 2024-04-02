library(tidyverse)

df <- read_tsv('data/running_wheel/cohort_1/20230801_093525_combined_replacment_wheels.txt', skip = 10) %>% 
  mutate(Bin = lubridate::mdy_hm(Bin) - hours(6) - minutes(30)) %>% 
  separate(Bin, into = c('date', 'time'), sep = " ") %>% 
  separate(time, into = c('hour', 'min', 'sec'), sep = ":") %>% 
  select(-sec) %>% 
  pivot_longer(-c(date, hour, min), names_to = 'cage', values_to = 'revolutions') %>% 
  separate(cage, into = c('drop', 'cage', 'gm', 'sex'), sep = "_") %>% 
  mutate(cage = paste0('cohort1_', cage))

day_list_1 <- df %>% 
  select(date) %>% 
  distinct() %>% 
  mutate(day = row_number())

df_1_day <- df %>% 
  left_join(., day_list_1)

metadata_1 <- df_1_day %>% 
  select(cage, gm, sex) %>% 
  distinct() %>% 
  drop_na()

df_2 <- read_tsv('data/running_wheel/cohort_2/20230823_081246.txt', skip = 10) %>% 
  mutate(Bin = lubridate::mdy_hm(Bin) - hours(6) - minutes(30)) %>% 
  separate(Bin, into = c('date', 'time'), sep = " ") %>% 
  separate(time, into = c('hour', 'min', 'sec'), sep = ":") %>% 
  select(-sec) %>% 
  pivot_longer(-c(date, hour, min), names_to = 'cage', values_to = 'revolutions') %>% 
  separate(cage, into = c('drop', 'cage', 'gm', 'sex'), sep = "_") %>% 
  mutate(cage = paste0('cohort2_', cage))

day_list_2 <- df_2 %>% 
  select(date) %>% 
  distinct() %>% 
  mutate(day = row_number())

df_2_day <- df_2 %>% 
  left_join(., day_list_2)

metadata_2 <- df_2_day %>% 
  select(cage, gm, sex) %>% 
  distinct() %>% 
  drop_na()

circumference = 0.000378


