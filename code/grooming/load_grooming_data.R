{
  library(tidyverse)
  library(readxl)
}

df1_id <- read_tsv('./data/non_CF/data/2208_cohort/testing_order/mice_test_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
df_1 <- read_excel('./data/non_CF/data/2208_cohort/grooming/grooming_data.xlsx') %>% 
  rename(randomid = 'random_id',
         time_grooming = 'time_grooming_ZM') %>% 
  inner_join(., df1_id, by = 'randomid') %>% 
  mutate(cohort = 1)

df2_id <- read_tsv('./data/non_CF/data/2210_cohort/testing_order/mice_test_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
df_2 <- read_excel('./data/non_CF/data/2210_cohort/grooming_videos/grooming_results.xlsx') %>% 
  inner_join(., df2_id, by = 'randomid') %>% 
  mutate(cohort = 2)


df_comb <- rbind(df_1, df_2) %>% 
  mutate(gm = case_when(gm == 1 ~ "GM1",
                        gm == 4 ~ "GM4"),
         sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male"),
         grooming_index = time_grooming / 600)



cf_df1_id <- read_tsv('./data/CF/data/2212_cohort/mice_test_order_full.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
cf_df_1 <- read_excel('./data/CF/data/2212_cohort/grooming/grooming_behavior.xlsx') %>% 
  rename(time_grooming = 'time_s') %>% 
  inner_join(., cf_df1_id, by = 'randomid') %>% 
  mutate(cohort = 1)

cf_df2_id <- read_tsv('./data/CF/data/2304_cohort/mouse_testing_order.tsv') %>% 
  select(strain, gm, sex, cage, randomid)
cf_df_2 <- read_excel('./data/CF/data/2304_cohort/grooming_time.xlsx') %>% 
  rename(time_grooming = 'time') %>% 
  inner_join(., cf_df2_id, by = 'randomid') %>% 
  mutate(cohort = 2)

cf_df_comb <- rbind(cf_df_1, cf_df_2) %>% 
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male"),
         grooming_index = time_grooming / 600)

