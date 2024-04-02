{
  library(tidyverse)
  library(readxl)
  library(outliers)
  library(rstatix)
}

df_1 <- read_excel('data/non_CF/data/2208_cohort/mouse_list_copy_latest.xlsx') %>% 
  select(mouse, strain, sex, gm, cage, d21_weight, d50_weight) %>% 
  mutate(cohort = 1) 

df_2 <- read_excel('data/non_CF/data/2210_cohort/mouse_list_copy_latest.xlsx') %>% 
  select(mouse, strain, sex, gm, cage, d21_weight, d50_weight) %>% 
  mutate(cohort = 2)

df_comb <- rbind(df_1, df_2)

df_comb$gm <- as.factor(df_comb$gm)


cf_df_1 <- read_excel('data/CF/data/2212_cohort/mouse_metadata_weights_latest.xlsx') %>% 
  select(mouse, strain, sex, gm, cage, d21_weight, d50_weight) %>% 
  filter(strain == 'BTBR') %>% 
  mutate(cohort = 1) %>% 
  filter(mouse != 19)

cf_df_2 <- read_excel('data/CF/data/2304_cohort/metadata.xlsx') %>% 
  select(mouse, strain, sex, gm, cage, d21_weight, d50_weight) %>% 
  filter(strain == 'BTBR') %>% 
  mutate(cohort = 2)

cf_df_comb <- rbind(cf_df_1, cf_df_2)
cf_df_comb$d21_weight <- as.double(cf_df_comb$d21_weight)
cf_df_comb$d50_weight <- as.double(cf_df_comb$d50_weight)


birth_weights <- read_excel('data/non_CF/data/2303_birth_weight/btbr_birthweights.xlsx')


d7_weights <- read_excel('data/non_CF/data/2303_birth_weight/USV_files.xlsx') %>% 
  select(mouse, gm, sex, birth_dam, cf_dam, dob, d7_weight)
