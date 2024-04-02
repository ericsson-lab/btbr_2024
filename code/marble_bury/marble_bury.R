{
  library(tidyverse)
  library(readxl)
  library(ggprism)
  library(rstatix)
}

theme_set(ggprism::theme_prism())


df_1 <- read_excel('./data/non_CF/data/2208_cohort/marble_bury/marble_bury_time.xlsx')
metadata_1 <- read_tsv('./data/non_CF/data/2208_cohort/testing_order/mice_test_full.tsv') 

df_2 <- read_excel('./data/non_CF/data/2210_cohort/marble_bury/marble_bury_time.xlsx')

metadata_2 <- read_tsv('./data/non_CF/data/2210_cohort/testing_order/mice_test_full.tsv') 


df_1_comb <- df_1 %>% 
  left_join(., metadata_1) 

df_2_comb <- df_2 %>% 
  left_join(., metadata_2) 

comb <- rbind(df_1_comb, df_2_comb) %>% 
  mutate(burying_index = time_s / 300,
         gm = ifelse(gm == 1, 'GM1', 'GM4'),
         sex = ifelse(sex == 'M', 'Male', 'Female'))

comb %>%
  filter(strain == 'BTBR') %>% 
  ggplot(aes(x = gm, y = burying_index, color = gm, shape = sex)) +
  geom_point(size = 4, 
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +  
  stat_summary(geom = 'bar', fill = NA, color = 'black', 
               width = 0.5, position = position_dodge(0.8), linewidth = 1) +
  stat_summary(geom = 'errorbar', color = 'black', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.2, position = position_dodge(0.8), 
               linewidth = 1) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(17, 16), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Burying Index') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))

ggsave('plots/marble/ctl/burying_index.png', width = 5, height = 4, bg = 'white')

comb %>%
  filter(strain == 'BTBR') %>% 
  rstatix::anova_test(burying_index ~ gm * sex) %>% 
  clipr::write_clip()



comb %>%
  filter(strain == 'B6') %>% 
  ggplot(aes(x = gm, y = burying_index, color = gm, shape = sex)) +
  geom_point(size = 4, 
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +  
  stat_summary(geom = 'bar', fill = NA, color = 'black', 
               width = 0.5, position = position_dodge(0.8), linewidth = 1) +
  stat_summary(geom = 'errorbar', color = 'black', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.2, position = position_dodge(0.8), 
               linewidth = 1) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(17, 16), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Burying Index') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))

ggsave('plots/marble/b6/burying_index.png', width = 5, height = 4, bg = 'white')

comb %>%
  filter(strain == 'B6') %>% 
  rstatix::anova_test(burying_index ~ gm * sex)



comb %>%
  group_by(strain) %>% 
  get_summary_stats(burying_index)
  anova_test(burying_index ~ strain * gm * sex)


cf_df_1 <- read_excel('./data/CF/data/2212_cohort/marble_bury/bury_time.xlsx')
cf_metadata_1 <- read_tsv('./data/CF/data/2212_cohort/mice_test_order_full.tsv') 

cf_df_2 <- read_excel('./data/CF/data/2304_cohort/marble_bury_data.xlsx')
cf_metadata_2 <- read_tsv('./data/CF/data/2304_cohort/mouse_testing_order.tsv') 


cf_df_1_comb <- cf_df_1 %>% 
  left_join(., cf_metadata_1) 

cf_df_2_comb <- cf_df_2 %>% 
  left_join(., cf_metadata_2) 

cf_comb <- rbind(cf_df_1_comb, cf_df_2_comb) %>% 
  mutate(burying_index = time_s / 300,
         sex = ifelse(sex == 'M', 'Male', 'Female')) %>% 
  drop_na()

cf_comb %>% 
  filter(strain == 'BTBR') %>% 
  
  ggplot(aes(x = gm, y = burying_index, color = gm, shape = sex)) +
  geom_point(size = 4, stroke = 1.5,
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) + 
  stat_summary(geom = 'bar', fill = NA, color = 'black', 
               width = 0.5, position = position_dodge(0.8), linewidth = 1) +
  stat_summary(geom = 'errorbar', color = 'black', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.2, position = position_dodge(0.8), 
               linewidth = 1) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(2, 1), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Burying Index') +
  scale_x_discrete(labels = c(expression(bold('CF'['Low'])),
                              expression(bold('CF'['High']))))

ggsave('plots/marble/cf/burying_index.png', width = 5, height = 4, bg = 'white')


cf_comb %>%
  filter(strain == 'B6') %>% 
  ggplot(aes(x = gm, y = burying_index, color = gm, shape = sex)) +
  geom_point(size = 4, 
             position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +  
  stat_summary(geom = 'bar', fill = NA, color = 'black', 
               width = 0.5, position = position_dodge(0.8), linewidth = 1) +
  stat_summary(geom = 'errorbar', color = 'black', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.2, position = position_dodge(0.8), 
               linewidth = 1) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(17, 16), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Burying Index') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))

ggsave('plots/marble/b6/burying_index.png', width = 5, height = 4, bg = 'white')

cf_comb %>%
  filter(strain == 'BTBR') %>% 
  rstatix::anova_test(burying_index ~ gm * sex)


