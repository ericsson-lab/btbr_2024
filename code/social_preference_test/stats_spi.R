{
  source("code/social_preference_test/load_spt_data.R")
}
  
df_spi <- df_testing %>% 
  select(strain, gm, sex, spi)

df_spi %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(spi ~ gm * sex) %>% 
  clipr::write_clip()
  add_significance() %>% 
  write_tsv('stats/spt/spi_anova_strain_gm_sex.tsv')

df_spi %>% 
  group_by(strain) %>% 
  anova_test(spi ~ gm * sex) %>% 
  add_significance() %>% 
  write_tsv('stats/spt/spi_group_by_strain_anova_gm_sex.tsv')

df_spi %>% 
  group_by(strain) %>% 
  tukey_hsd(spi ~ gm * sex) %>% 
  add_significance() %>% 
  write_tsv('stats/spt/spi_group_by_strain_tukey_gm_sex.tsv')
