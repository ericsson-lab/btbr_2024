{
  source('code/social_preference_test/load_spt_data.R')
}


df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  ggplot(aes(x = sex, y = time, shape = position, color = gm)) +
  geom_point(aes(color = gm), size = 4, stroke = 1.5, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.7)) +
  stat_summary(fun = 'mean', geom = 'bar', width = 0.55, 
               position = position_dodge(0.7), color = 'black',
               fill = NA, linewidth = 1.5) +
  stat_summary(fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x),
               geom = 'errorbar', width = 0.2, position = position_dodge(0.7), 
               color = 'black',
               linewidth = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)),
                     limits = c(0, 600), 
                     breaks = c(seq(0, 600, 200))) +
  
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(1, 16), name = "Position",
                     labels = c("Object", "Stranger")) +    theme_classic() +
  labs(y = "Grooming Index (%)") +
  facet_wrap(~ gm, strip.position = 'bottom')  +
  
  ggprism::theme_prism() +
  labs(y = "Time in Zone (s)") +
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 18),
    axis.text = element_text(face = "bold", color = "black", size = 18),
    
    legend.text = element_text(face = "bold", color = "black", size = 18),
    legend.title = element_text(face = "bold", color = "black", 
                                size = 18,hjust = 0.5),
    legend.margin = margin(rep(0,4)),
    legend.box.margin = margin(0,0,0,-25),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(face = "bold", color = "black", size = 18),
    strip.placement = 'outside',
    
    panel.grid = element_blank(),
    panel.spacing.x=unit(0, "lines"),panel.spacing.y=unit(0, "lines"),
    aspect.ratio = 3/1.5
    
  ) +
  guides(
    color = 'none',
    shape = guide_legend(order = 2, direction = 'vertical', title.position = 'top')
  )

ggsave('plots/spt/ctl/spt_time.png', height = 5, width = 8)


df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  group_by(gm, sex) %>% 
  t_test(time ~ position, paired = T) %>% 
  select (gm, sex, p)

df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(time ~ position * gm * sex)
  


df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'B6') %>% 
  ggplot(aes(x = sex, y = time, shape = position, color = gm)) +
  geom_point(aes(color = gm), size = 4, stroke = 1.5, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.7)) +
  stat_summary(fun = 'mean', geom = 'bar', width = 0.55, 
               position = position_dodge(0.7), color = 'black',
               fill = NA, linewidth = 1.5) +
  stat_summary(fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x),
               geom = 'errorbar', width = 0.2, position = position_dodge(0.7), 
               color = 'black',
               linewidth = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)),
                     limits = c(0, 600), 
                     breaks = c(seq(0, 600, 200))) +
  
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(1, 16), name = "Position",
                     labels = c("Object", "Stranger")) +    theme_classic() +
  labs(y = "Grooming Index (%)") +
  facet_wrap(~ gm, strip.position = 'bottom')  +
  
  ggprism::theme_prism() +
  labs(y = "Time in Zone (s)") +
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 18),
    axis.text = element_text(face = "bold", color = "black", size = 18),
    
    legend.text = element_text(face = "bold", color = "black", size = 18),
    legend.title = element_text(face = "bold", color = "black", 
                                size = 18,hjust = 0.5),
    legend.margin = margin(rep(0,4)),
    legend.box.margin = margin(0,0,0,-25),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(face = "bold", color = "black", size = 18),
    strip.placement = 'outside',
    
    panel.grid = element_blank(),
    panel.spacing.x=unit(0, "lines"),panel.spacing.y=unit(0, "lines"),
    aspect.ratio = 3/1.5
    
  ) +
  guides(
    color = 'none',
    shape = guide_legend(order = 2, direction = 'vertical', title.position = 'top')
  )


ggsave('plots/spt/b6/spt_time.png', height = 5, width = 8)


df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'B6') %>% 
  group_by(gm, sex) %>% 
  t_test(time ~ position, paired = T) %>% 
  select (gm, sex, p)

df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'B6') %>% 
  anova_test(time ~ position * gm * sex)
  
  
  ## CF


cf_df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  ggplot(aes(x = sex, y = time, shape = position, color = gm)) +
  geom_point(aes(color = gm), size = 4, stroke = 1.5, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.7)) +
  stat_summary(fun = 'mean', geom = 'bar', width = 0.55, 
               position = position_dodge(0.7), color = 'black',
               fill = NA, linewidth = 1.5) +
  stat_summary(fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x),
               geom = 'errorbar', width = 0.2, position = position_dodge(0.7), 
               color = 'black',
               linewidth = 1.5) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)),
                     limits = c(0, 600), 
                     breaks = c(seq(0, 600, 200))) +
  
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(1, 16), name = "Position",
                     labels = c("Object", "Stranger")) +    theme_classic() +
  labs(y = "Grooming Index (%)") +
  facet_wrap(~ gm, strip.position = 'bottom')  +
  
  ggprism::theme_prism() +
  labs(y = "Time in Zone (s)") +
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 18),
    axis.text = element_text(face = "bold", color = "black", size = 18),
    
    legend.text = element_text(face = "bold", color = "black", size = 18),
    legend.title = element_text(face = "bold", color = "black", 
                                size = 18,hjust = 0.5),
    legend.margin = margin(rep(0,4)),
    legend.box.margin = margin(0,0,0,-25),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(face = "bold", color = "black", size = 18),
    strip.placement = 'outside',
    
    panel.grid = element_blank(),
    panel.spacing.x=unit(0, "lines"),panel.spacing.y=unit(0, "lines"),
    aspect.ratio = 3/1.5
    
  ) +
  guides(
    color = 'none',
    shape = guide_legend(order = 2, direction = 'vertical', title.position = 'top')
  )

ggsave('plots/spt/cf/spt_time.png', height = 5, width = 8)

cf_df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  group_by(gm, sex) %>% 
  t_test(time ~ position, paired = T) %>% 
  select (gm, sex, p)

cf_df_testing %>% 
  select(strain, gm, sex, stranger_time, object_time, cohort) %>% 
  pivot_longer(-c(strain, gm, sex, cohort), 
               names_to = 'position',
               values_to = 'time') %>% 
  filter(strain == 'BTBR') %>% 
  anova_test(time ~ position * gm * sex)

