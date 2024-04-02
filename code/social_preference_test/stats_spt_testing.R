{
  source('code/social_preference_test/load_spt_data.R')

  library(rstatix)
  library(gt)
}

df_testing %>% 
  select(strain, gm, sex, stranger_time, 
         object_time) %>% 
  filter(strain == 'BTBR') %>% 
  pivot_longer(-c(strain, gm, sex), 
               names_to = 'position', 
               values_to = 'time') %>% 
  group_by(strain, gm, sex) %>% 
  wilcox_test(time ~ position,paired = T) %>% 
  select(gm, sex, group1, group2, n1, n2, p)
  clipr::write_clip()
  
  add_significance() %>% 
  write_tsv('stats/spt/chamber_time_t_test_by_strain.tsv')

df_testing %>% 
  select(strain, gm, sex, stranger_time, 
         object_time) %>% 
  pivot_longer(-c(strain, gm, sex), 
               names_to = 'position', 
               values_to = 'time') %>% 
  group_by(strain, gm, sex) %>% 
  t_test(time ~ position, paired = T) %>% 
  add_significance() %>% 
  arrange(p) %>% 
  select(-c(.y., group1, group2, n1, n2)) %>% 
  gt() %>% 
  gtsave('stats/spt/table_figures/chamber_time_t_test_by_strain.png')

df_testing %>% 
  select(strain, gm, sex, stranger_time, 
         object_time) %>% 
  filter(strain == "BTBR") %>% 
  pivot_longer(-c(strain, gm, sex), 
               names_to = 'position', 
               values_to = 'time') %>% 
  anova_test(time ~ position * sex * gm)



df_testing %>% 
  select(strain, gm, sex, stranger_time, 
         object_time) %>% 
  pivot_longer(-c(strain, gm, sex), 
               names_to = 'position', 
               values_to = 'time') %>% 
  filter(strain == 'B6') %>% 
  # group_by(strain, gm, sex) %>%
  anova_test(time ~position *gm*sex)
  # t_test(time ~ position, paired = T)
  