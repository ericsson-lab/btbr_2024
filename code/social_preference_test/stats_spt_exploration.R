{
  source('code/social_preference_test/load_spt_data.R')
  
  library(rstatix)
  library(gt)
}
  
df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  anova_test(distance ~ strain * gm * sex) %>% 
  write_tsv('stats/spt/exploration_distance_anova.tsv')
df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  anova_test(distance ~ strain * gm * sex) %>% 
  gt() %>% 
  gtsave('stats/spt/table_figures/exploration_distance_anova.png')

df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  tukey_hsd(distance ~ strain * gm * sex) %>% 
  write_tsv('stats/spt/exploration_distance_tukey.tsv')
df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  tukey_hsd(distance ~ strain * gm * sex) %>% 
  gt() %>% 
  gtsave('stats/spt/table_figures/exploration_distance_tukey.png')



df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  group_by(strain) %>% 
  anova_test(distance ~ gm * sex) %>% 
  write_tsv('stats/spt/exploration_distance_anova_by_strain.tsv')
df_exploration %>% 
  select(strain, gm, sex, distance) %>% 
  group_by(strain) %>% 
  anova_test(distance ~ gm * sex) %>% 
  gt() %>% 
  gtsave('stats/spt/table_figures/exploration_distance_anova_by_strain.png')
