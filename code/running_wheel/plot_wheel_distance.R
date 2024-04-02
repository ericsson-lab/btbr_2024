df_1_day %>% 
  rbind(., df_2_day) %>% 
  filter(day <=7) %>% 
  drop_na() %>% 
  group_by(day, cage, gm, sex) %>% 
  summarize(revolutions = sum(revolutions)) %>% 
  mutate(km = revolutions * circumference,
         sex = str_to_sentence(sex)) %>%
  ggplot(aes(x = day, y = km, color = gm, linetype = sex, fill = gm)) +
   stat_summary(fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) ifelse(mean(x) - sd(x) < 0, 0, mean(x) - sd(x)), 
               geom = 'ribbon',  color = NA, alpha = 0.2, show.legend = F)  +
  stat_summary(fun = 'mean', geom = 'line',
               linewidth = 1.5) +
  ggprism::theme_prism() +
  scale_y_continuous(expand = expansion(mult = c(0,0.01)), limits = c(0, 16)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = c('red', 'dodgerblue'), name = 'GM',
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])))) +  
  scale_fill_manual(values = c('red', 'dodgerblue'), name = 'GM',) +
  scale_linetype(name = 'Sex') +
  labs(y = 'Distance Travelled (km)',
       x = 'Day') +
  theme(
    aspect.ratio = 2/3, 
    legend.title = element_text(hjust = 0),
    legend.text = element_text(face = 'bold', color = 'black', 
                               hjust = 0, size = 16)
  )

ggsave('plots/running_wheel/distance_travelled.png', width = 6, height = 4)
  
df_1_day %>% 
  rbind(., df_2_day) %>% 
  filter(day <=7) %>% 
  group_by(day, cage, gm, sex) %>% 
  drop_na() %>% 
  summarize(revolutions = sum(revolutions), .groups = 'drop') %>% 
  mutate(km = revolutions * circumference) %>% 
  rstatix::anova_test(km ~ gm * sex * factor(day))
 

df_1_day %>% 
  rbind(., df_2_day) %>% 
  filter(day <=7) %>% 
  group_by(day, cage, gm, sex) %>% 
  drop_na() %>% 
  summarize(revolutions = sum(revolutions), .groups = 'drop') %>% 
  mutate(km = revolutions * circumference) %>% 
  rstatix::tukey_hsd(km ~ gm * sex * factor(day)) %>% 
  filter(p.adj < 0.05)

