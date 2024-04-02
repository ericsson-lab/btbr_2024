{
  source('code/grooming/load_grooming_data.R')
  library(ggprism)
  library(rstatix)
}


df_comb %>% 
  filter(strain == 'BTBR') %>% 
  ggplot(aes(x = gm, y = grooming_index, color = gm, shape = sex)) +
  geom_point(aes(color = gm), size = 4, stroke = 1.5,
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Grooming Index') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))

ggsave('plots/grooming/ctl/ctl_grooming_index.png', width = 5, height = 4)



df_comb %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(grooming_index ~ gm * sex)
df_comb %>% 
  filter(strain == 'BTBR') %>% 
  tukey_hsd(grooming_index ~ gm * sex) %>% 
  select(group1, group2, p.adj)



df_comb %>% 
  filter(strain == 'B6') %>% 
  ggplot(aes(x = gm, y = grooming_index, color = gm, shape = sex)) +
  geom_point(aes(color = gm), size = 4, 
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Grooming Index') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))
  
ggsave('plots/grooming/b6/b6_grooming_index.png', width = 5, height = 4)

df_comb %>% 
  filter(strain == 'B6') %>% 
  anova_test(grooming_index ~ gm * sex) 


cf_df_comb %>% 
  filter(strain == 'BTBR') %>% 
  ggplot(aes(x = gm, y = grooming_index, color = gm, shape = sex)) +
  geom_point(aes(color = gm), size = 4, stroke = 1.5,
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     labels = scales::percent) +
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(color = 'none') +
  labs(y = 'Grooming Index') +
  scale_x_discrete(labels = c(expression(bold('CF'['Low'])),
                              expression(bold('CF'['High']))))

ggsave('plots/grooming/cf/cf_grooming_index.png', width = 5, height = 4)


cf_df_comb %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(grooming_index ~ gm * sex) 

cf_df_comb %>% 
  group_by(strain, gm, sex) %>% 
  get_summary_stats(grooming_index)
anova_test(grooming_index ~ gm * sex) 

  