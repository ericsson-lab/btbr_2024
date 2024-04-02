{
  library(tidyverse)
  library(readxl)
  library(janitor)
}

df1_id <- read_tsv('data/non_CF/data/2208_cohort/testing_order/mice_test_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
df1 <- read_excel('data/non_CF/data/2208_cohort/social_preference/compiled_data/comb_data.xlsx') %>% 
  inner_join(., df1_id, by = "randomid") %>% 
  mutate(cohort = 1)

df2_id <- read_tsv('data/non_CF/data/2210_cohort/testing_order/mice_test_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
df2 <- read_csv('data/non_CF/data/2210_cohort/social_preference_test/comb/comb_data.csv') %>% 
  inner_join(., df2_id, by = "randomid") %>% 
  mutate(cohort = 2)

df_combined <- rbind(df1, df2) %>% 
  clean_names() %>% 
  mutate(gm = case_when(gm == 1 ~ "GM1",
                        gm == 4 ~ "GM4"),
         sex = case_when(sex == "M" ~ "Male",
                         sex == "F" ~ "Female"))

df_combined$cohort <- as.factor(df_combined$cohort)

df_testing <- df_combined %>% 
  filter(stage == "TESTING") %>% 
  mutate(stranger_time = case_when(stranger_location == "Front" ~ front_time,
                                   stranger_location == "Back" ~ back_time),
         object_time = case_when(object_location == "Front" ~ front_time,
                                 object_location == "Back" ~ back_time),
         stranger_entry = case_when(stranger_location == "Front" ~ front_entries,
                                    stranger_location == "Back" ~ back_entries),
         object_entry = case_when(object_location == "Front" ~ front_entries,
                                  object_location == "Back" ~ back_entries),
         spi = ((stranger_time - object_time) / (stranger_time + object_time)))
  

df_exploration <- df_combined %>% 
  filter(stage == "EXPLORATION") 





cf_df1_id <- read_tsv('data/CF/data/2212_cohort/mice_test_order_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
cf_df1 <- read_excel('data/CF/data/2212_cohort/spt/comb_data.xlsx') %>% 
  inner_join(., cf_df1_id, by = "randomid") %>% 
  mutate(cohort = 1) %>% 
  clean_names() %>% 
  rename(distance = 'distance_m') %>% 
  select(-test_date)

cf_df2_id <- read_tsv('data/CF/data/2304_cohort/mouse_testing_order.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
cf_df2 <- read_csv('data/CF/data/2304_cohort/spt/comb_spt.csv') %>% 
  inner_join(., cf_df2_id, by = "randomid") %>% 
  mutate(cohort = 2) %>% 
  clean_names() 

cf_df_combined <- rbind(cf_df1, cf_df2) %>% 
  mutate(sex = case_when(sex == "M" ~ "Male",
                         sex == "F" ~ "Female"))

cf_df_combined$cohort <- as.factor(cf_df_combined$cohort)

cf_df_testing <- cf_df_combined %>% 
  filter(stage == "TESTING") %>% 
  mutate(stranger_time = case_when(stranger_location == "Front" ~ front_time,
                                   stranger_location == "Back" ~ back_time),
         object_time = case_when(object_location == "Front" ~ front_time,
                                 object_location == "Back" ~ back_time),
         stranger_entry = case_when(stranger_location == "Front" ~ front_entries,
                                    stranger_location == "Back" ~ back_entries),
         object_entry = case_when(object_location == "Front" ~ front_entries,
                                  object_location == "Back" ~ back_entries),
         spi = ((stranger_time - object_time) / (stranger_time + object_time)))


cf_df_exploration <- cf_df_combined %>% 
  filter(stage == "EXPLORATION") 
