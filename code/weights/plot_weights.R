{
  library(glue)
  
  source('code/weights/load_weights.R')
}

plot_weight <- function(df, strain, weight, y_lab){
  df %>% 
    mutate(gm = case_when(gm == 1 ~ "GM1",
                          gm == 4 ~ "GM4")) %>% 
    filter(strain %in% {{strain}}) %>% 
    ggplot(aes(x = gm , y = {{weight}}, shape = sex, color = gm)) +
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
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(y = y_lab) +
    theme(
      aspect.ratio = 3/2,
      axis.title.x = element_blank(),
      axis.text = element_text(size = 18),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(color = "black", face = "bold", size = 16),
      legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
    ) +
    guides(color = 'none') +
    scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High']))))
}  

plot_weight(df = df_comb, strain = 'BTBR', weight = d21_weight, y_lab = 'D21 Weight') 

ggsave('plots/weights/ctl_d21.png', height = 4, width = 5)


df_comb %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(d21_weight ~ gm * sex)


plot_weight(df = df_comb, strain = 'BTBR', weight = d50_weight, y_lab = 'D50 Weight') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 35))

ggsave('plots/weights/ctl_d50.png', height = 4, width = 5)

df_comb %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(d50_weight ~ gm * sex)


plot_weight(df = df_comb, strain = 'B6', weight = d21_weight, y_lab = 'D21 Weight') 
ggsave('plots/weights/b6_d21.png', height = 4, width = 5)

df_comb %>% 
  filter(strain == 'B6') %>% 
  anova_test(d21_weight ~ gm * sex)

plot_weight(df = df_comb, strain = 'B6', weight = d50_weight, y_lab = 'D50 Weight') 
ggsave('plots/weights/b6_d50.png', height = 4, width = 5)
df_comb %>% 
  filter(strain == 'B6') %>% 
  anova_test(d50_weight ~ gm * sex)





cf_plot_weight <- function(df, weight, y_lab){
  cf_df_comb %>% 
    filter(strain %in% 'BTBR') %>% 
    ggplot(aes(x = gm , y = {{weight}}, shape = sex, color = gm)) +
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
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(y = y_lab) +
    theme(
      aspect.ratio = 3/2,
      axis.title.x = element_blank(),
      axis.text = element_text(size = 18),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(color = "black", face = "bold", size = 16),
      legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
    ) +
    guides(color = 'none') +
    scale_x_discrete(labels = c(expression(bold('CF'['Low'])),
                                expression(bold('CF'['High']))))
}  

cf_plot_weight(df = cf_df_comb, weight = d21_weight, y_lab = 'D21 Weight') 
ggsave('plots/weights/cf_d21.png', height = 4, width = 5)
cf_df_comb %>% 
  filter(strain == 'BTBR') %>%
  anova_test(d21_weight ~ gm * sex)
cf_df_comb %>% 
  count(gm, sex)
cf_plot_weight(df = cf_df_comb, weight = d50_weight, y_lab = 'D50 Weight') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 35))

ggsave('plots/weights/cf_d50.png', height = 4, width = 5)
cf_df_comb %>% 
  filter(strain == 'BTBR') %>%
  anova_test(d50_weight ~ gm * sex)


birth_weights %>% 
  ggplot(aes(x = gm , y = birth_weight, shape = sex, color = gm)) +
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
  scale_shape_manual(values = c(17, 16), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  theme(
    aspect.ratio = 3/2,
    axis.title.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  ylab('Birth Weight (g)') +
  guides(color = 'none') +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))

ggsave('plots/weights/birth_weight.png', width = 5, height = 4)

birth_weights %>% 
  anova_test(birth_weight ~ gm * sex)



plot_d7_weight <- function(gm){
  d7_weights %>% 
    filter(gm %in% {{gm}}) %>%  
    ggplot(aes(x = gm , y = d7_weight, shape = sex, color = gm)) +
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

    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    
    theme(
      aspect.ratio = 3/2,
      axis.title.x = element_blank(),
      axis.text = element_text(size = 18),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(color = "black", face = "bold", size = 16),
      legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
    ) +
    ylab('D7 Weight (g)') +
    guides(color = 'none') +
    scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High']))))
}
plot_d7_weight(gm = c('GM1', 'GM4')) +
  scale_shape_manual(values = c(17, 16), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High']))))
ggsave('plots/weights/ctl_d7.png', height = 4, width = 5)


d7_weights %>% 
  filter(gm %in% c('GM1', 'GM4')) %>% 
  anova_test(d7_weight ~ gm * sex)

plot_d7_weight(gm = c('CF1', 'CF4')) +
  scale_shape_manual(values = c(2, 1), name = 'Sex',
                     labels = c('Female', 'Male')) +
  scale_x_discrete(labels = c(expression(bold('CF'['Low'])),
                              expression(bold('CF'['High']))))
ggsave('plots/weights/cf_d7.png', height = 4, width = 5)





  