
source('code/load_data.R')




day_list <- df %>% 
  select(date) %>% 
  distinct() %>%
  mutate(day = row_number())

metadata <- df %>% 
  select(cage, gm, sex) %>% 
  distinct() %>% 
  drop_na() %>% 
  mutate(cage = as.double(cage))

circumference = 0.000378

cage_data <- df_2 %>% 
  filter(cage %in% metadata_2$cage) %>% 
  left_join(., day_list_2) %>% 
  group_by(date, hour, cage, day) %>% 
  drop_na() %>% 
  summarise(rev_per_hour = sum(revolutions), .groups = 'drop') %>% 
  mutate(km_per_day = rev_per_hour * circumference) %>% 
  left_join(., metadata_2, by = 'cage') %>% 
  separate(cage, into = c('cohort', 'cage'), sep = '_') %>% 
  mutate(time = paste(date, hour),
         cage = as.double(cage)) %>% 
  mutate(color = case_when(gm == 'GM1' ~ 'red', 
                           gm == 'GM4' ~ 'dodgerblue'))

metadata
plot_cage <- function(cage_num){
  cage_data %>% 
    filter(cage == cage_num) %>% 
    # filter(day >= 6 & day <= 10) %>% 
    ggplot(aes(x = time, y = rev_per_hour, group = cage, color = color)) +
    geom_line() +
    scale_color_identity() +
    scale_y_continuous(limits = c(0, 5100)) +
    theme(
      axis.text = element_text(angle = 90)
    )
  ggsave(glue::glue('cohort_2/plots/individual_cages/cage_{cage_num}.png'), width = 40, height = 4)
}
for (i in 1:24) {
  plot_cage(i)
}


count <- readxl::read_excel('~/Desktop/Book1.xlsx')
count %>% 
  summarise(mean = mean(day),
            sd = sd(day))
count %>% 
  ggplot(aes(x = day)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(7, 15)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 15)) +
  ggprism::theme_prism() +
  labs(y = 'Wheels', x = 'Days To Failure')
ggsave('~/Desktop/days_to_failure.png', width = 5, height = 5)

cage_data %>% 
  group_by(hour, cage) %>% 
  # filter(day >= 6 & day <= 10) %>% 
  summarise(rev_per_day = sum(rev_per_hour), .groups = 'drop') %>% 
  mutate(km_per_day = rev_per_day * circumference) %>% 
  drop_na() %>% 
  left_join(., metadata) %>% 
  # filter(date < '2023-08-11' & date > '2023-08-04') %>% 
  # rstatix::anova_test(km_per_day ~ gm * sex)
  # write_tsv('cohort_1/stats.tsv')
  ggplot(aes(x = hour, y = km_per_day, color = gm, linetype = sex)) +
  geom_line(aes(group = cage), show.legend = F, alpha = 0.5) +
  stat_summary(geom = 'line', fun = 'mean', linewidth = 2, aes(group = interaction(gm, sex))) +
  scale_color_manual(values = c('red', 'dodgerblue'),
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High']))),
                     name = 'GM') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c('Female', 'Male'),
                        name = 'Sex') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # scale_x_discrete(expand = c(0,0), labels = c(5,6,7,8,9,10)) +
  labs(x = 'Day',
       y = 'Distance Travelled (km)') +
  ggprism::theme_prism() +
  theme(
    aspect.ratio = 2/2.5,
    legend.text = element_text(face = 'bold', color = 'black', hjust = 0),
    legend.title = element_text(face = 'bold', color = 'black', hjust = 0), 
  ) + 
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  )

ggsave('cohort_2/distance_travelled_by_day.png', width = 8, height = 5)




df %>% 
  filter(date < '2023-08-11' & date > '2023-08-04') %>% 
  group_by(hour, cage) %>% 
  drop_na() %>% 
  summarise(rev_per_hour = sum(revolutions), .groups = 'drop') %>% 
  mutate(km_per_day = rev_per_hour * circumference) %>% 
  left_join(., metadata, by = 'cage') %>% 
  # rstatix::anova_test(km_per_day ~ gm * sex * hour)
  # write_tsv('cohort_1/stats.tsv')
  ggplot(aes(x = hour, y = km_per_day, color = gm, linetype = sex)) +
  # geom_rect(aes(xmin = 12, xmax = 23, ... = ))
  geom_line(aes(group = cage), show.legend = F, alpha = 0.5) +
  stat_summary(geom = 'line', fun = 'mean', linewidth = 2, aes(group = interaction(gm, sex))) +
  scale_color_manual(values = c('red', 'dodgerblue'),
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High']))),
                     name = 'GM') +
  scale_linetype_manual(values = c(1, 2),
                        labels = c('Female', 'Male'),
                        name = 'Sex') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # scale_x_discrete(expand = c(0,0), labels = c(5,6,7,8,9,10)) +
  labs(x = 'Day',
       y = 'Distance Travelled (km)') +
  ggprism::theme_prism() +
  theme(
    aspect.ratio = 2/2.5,
    legend.text = element_text(face = 'bold', color = 'black', hjust = 0),
    legend.title = element_text(face = 'bold', color = 'black', hjust = 0), 
  ) + 
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  )

ggsave('cohort_1/distance_travelled_by_hour.png', width = 8, height = 5)

