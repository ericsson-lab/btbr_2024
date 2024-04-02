library(tidyverse)
library(readxl)

df <- read_excel('cohort_1/pre_post_weights.xlsx') %>% 
  mutate(delta = pre_weight - post_weight)

df_2 <- read_excel('cohort_2/pre_post_weights.xlsx') %>% 
  mutate(delta = pre_weight - post_weight)

df_comb <- df %>% 
  rbind(., df_2) 
  
df_comb %>% 
  mutate(sex = case_match(sex,
                          'F' ~ 'Female',
                          'M' ~ 'Male'),
         gm = case_match(gm, 
                         4 ~ 'GMHigh',
                         1 ~ 'GMLow'),
         gm = factor(gm, levels = c('GMLow', 'GMHigh'))) %>% 
  ggplot(aes(x = factor(gm), y = post_weight, color = gm, 
             shape = sex)) +
  stat_summary(geom = 'bar', width = 0.3,
               position = position_dodge(0.5),
               fill = NA, color = 'black', 
               linewidth = 1) +
  geom_point(position = position_jitterdodge(dodge.width = 0.5,
                                             jitter.width = 0.2),
             size = 3) +
  stat_summary(geom = 'errorbar', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.1, color = 'black',
               position = position_dodge(0.5),
               linewidth = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(0, 41)) +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High'])))) +
  scale_color_manual(values = c('red', 'dodgerblue')) +
  scale_shape_manual(values = c(16, 17), name = 'Sex') +
  labs(y = 'Weight (g)') +
  ggprism::theme_prism() +
  theme(
    aspect.ratio = 3/2,
    legend.text = element_text(face = 'bold', size = 14),
    legend.title = element_text(face = 'bold', size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14)
  ) +
  guides(
    color = 'none'
  )
  
ggsave('post_wheel_weights.png', width = 6, height = 5, bg = 'white')
  
  

df_comb %>% 
  mutate(sex = case_match(sex,
                          'F' ~ 'Female',
                          'M' ~ 'Male'),
         gm = case_match(gm, 
                         4 ~ 'GMHigh',
                         1 ~ 'GMLow'),
         gm = factor(gm, levels = c('GMLow', 'GMHigh'))) %>% 
  ggplot(aes(x = factor(gm), y = pre_weight, color = gm, 
             shape = sex)) +
  stat_summary(geom = 'bar', width = 0.3,
               position = position_dodge(0.5),
               fill = NA, color = 'black', 
               linewidth = 1) +
  geom_point(position = position_jitterdodge(dodge.width = 0.5,
                                             jitter.width = 0.2),
             size = 3) +
  stat_summary(geom = 'errorbar', 
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.1, color = 'black',
               position = position_dodge(0.5),
               linewidth = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(0, 41)) +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High'])))) +
  scale_color_manual(values = c('red', 'dodgerblue')) +
  scale_shape_manual(values = c(16, 17), name = 'Sex') +
  labs(y = 'Weight (g)') +
  ggprism::theme_prism() +
  theme(
    aspect.ratio = 3/2,
    legend.text = element_text(face = 'bold', size = 14),
    legend.title = element_text(face = 'bold', size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14)
  ) +
  guides(
    color = 'none'
  )

ggsave('pre_wheel_weights.png', width = 6, height = 5, bg = 'white')




df_comb %>% 
  mutate(sex = case_match(sex,
                          'F' ~ 'Female',
                          'M' ~ 'Male'),
         gm = case_match(gm, 
                         4 ~ 'GMHigh',
                         1 ~ 'GMLow'),
         gm = factor(gm, levels = c('GMLow', 'GMHigh'))) %>% 
  mutate(pct_weight_loss = 1 - (post_weight / pre_weight)) %>% 
  # rstatix::anova_test(pct_weight_loss ~ gm * sex)
  ggplot(aes(x = factor(gm), y = pct_weight_loss, color = gm, 
             shape = sex)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.6,
                                             jitter.width = 0.2),
             size = 3) +
  geom_boxplot(fill = NA, color = 'black', width = 0.4, 
               position = position_dodge(0.6), 
               linewidth = 1, outlier.colour = NA, 
               show.legend = F) + 
  # scale_y_continuous(expand = expansion(mult = c(0.01, 0.01)),
                     # limits = c(-1, 5)) +
  scale_x_discrete(labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High'])))) +
  scale_color_manual(values = c('red', 'dodgerblue')) +
  scale_shape_manual(values = c(16, 17), name = 'Sex') +
  labs(y = 'Weight (g)') +
  ggprism::theme_prism() +
  theme(
    aspect.ratio = 3/2,
    legend.text = element_text(face = 'bold', size = 14),
    legend.title = element_text(face = 'bold', size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14)
  ) +
  guides(
    color = 'none',
    fill = 'none',
  )
ggsave('delta_weights.png', width = 6, height = 5, bg = 'white')





df_comb %>% 
  rstatix::anova_test(pre_weight ~ gm * sex) %>% 
  as_tibble() %>% 
  select(Effect, F, p, `p<.05`) %>% 
  clipr::write_clip()
df_comb %>% 
  rstatix::anova_test(post_weight ~ gm * sex) %>% 
  as_tibble() %>% 
  select(Effect, F, p, `p<.05`) %>% 
  clipr::write_clip()
df_comb %>% 
  rstatix::anova_test(delta ~ gm * sex) %>% 
  as_tibble() %>% 
  select(Effect, F, p, `p<.05`) %>% 
  clipr::write_clip()



df %>% 
  pivot_longer(-c(cage, gm, sex), 
               names_to = 'time', 
               values_to = 'weight') %>% 
  ggplot(aes(x = time, y = weight, color = gm, 
             shape = sex)) +
  geom_line(aes(group = cage)) +
  geom_point() 
