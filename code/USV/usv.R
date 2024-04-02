{
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(ggprism)
}

metadata <- read_excel('./data/USVs/USV_files.xlsx') %>% 
  filter(mouse != 12) # Mouse found dead.

# files_list <- metadata %>% 
#   mutate(USV_file = str_replace_all(USV_file, ".wav", ".xlsx")) %>% 
#   pull(USV_file)
#
# process_counts <- function(file) {
#   read_excel(glue::glue('all_result_files/{file}')) %>% 
#   clean_names() %>% 
#   filter(end_time < 300) %>% 
#   mutate(file_name = file) %>% 
#   group_by(file_name, class) %>% 
#   count() %>% 
#   pivot_wider(names_from = 'class', values_from = 'n')
#   
# }
# 
# output <- lapply(files_list, process_counts)
# usv_table <- do.call(rbind, output)
# 
# write_rds(usv_table, file = 'usv_table.RDS')

usv_table <- read_rds('./data/USVs/usv_table.RDS')

usv_classes <- usv_table %>% 
  ungroup() %>% 
  select(-file_name) %>% 
  colnames()

table_metadata <- usv_table %>% 
  ungroup() %>% 
  mutate(file_name = str_replace_all(file_name, ".xlsx", ".wav"),
         USV_file = file_name) %>% 
  left_join(., metadata) %>% 
  select(mouse, gm, sex, any_of(usv_classes)) %>% 
  pivot_longer(-c(mouse, gm, sex), names_to = 'class', values_to = 'n') %>% 
  mutate(n = replace_na(n, 0))

  
table_metadata$gm <- factor(table_metadata$gm, levels = c('GM1', 'GM4', 'CF1', 'CF4'))


ctl_usv_rate <- table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>% 
  drop_na() %>% 
  summarize(total_usv = sum(n), .groups = 'drop') %>% 
  filter(gm %in% c('GM1', 'GM4')) %>% 
  mutate(usv_rate = total_usv/5)

table_metadata %>% 
  mutate(sex = case_when(sex == 'M' ~ 'Male',
                         sex == 'F' ~ 'Female')) %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>% 
  drop_na() %>% 
  summarize(total_usv = sum(n), .groups = 'drop') %>% 
  filter(gm %in% c('GM1', 'GM4')) %>%
  ggplot(aes(x = gm, y = total_usv/5, shape = sex, color = gm)) +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(breaks = c('GM1', 'GM4'),
                   labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High'])))) +
  labs(y = "Call Rate (USVs/min)") +
  theme_prism() +
  theme(
    axis.title.x = element_blank(),
    legend.text = element_text(face = 'bold', color = 'black', size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 16),
    
    aspect.ratio = 3/2
  ) +
  guides(
    color = 'none'
  )

ggsave(filename = 'plots/USV/ctl/ctl_usv_calls_per_min.png', width = 5, height = 4, bg = 'white')

ctl_usv_rate %>% 
  rstatix::anova_test(usv_rate ~ gm * sex) 

ctl_usv_rate %>% 
  rstatix::tukey_hsd(usv_rate ~ gm * sex) %>% 
  select(group1, group2, p.adj)


usv_dist <- table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  filter(gm %in% c('GM1', 'GM4')) %>%
  mutate(sampleid = paste(gm, sex, mouse, sep = '_')) %>% 
  group_by(mouse) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  mutate(n_rel_abund = (n/sum(n))*100) %>% 
  select(sampleid, class, n_rel_abund) %>% 
  pivot_wider(names_from = "class", values_from = 'n_rel_abund') %>% 
  column_to_rownames(var = 'sampleid') %>% 
  vegan::vegdist(method = 'bray')

usv_pcoa <- ape::pcoa(usv_dist, correction = 'cailliez')

metadata$mouse <- as.character(metadata$mouse)

pcoa_data <- usv_pcoa$vectors %>% 
  as_tibble(rownames = 'sampleid') %>% 
  separate(col = sampleid, into = c('gm', 'sex', 'mouse'), sep = '_') 

usv_pcoa
usv_pcoa 

p_var <- (usv_pcoa$values$Eigenvalues/usv_pcoa$trace)*100

pcoa_data$gm <- factor(pcoa_data$gm, levels = c('GM1', 'GM4', 'CF1', 'CF4'))

pcoa_data %>% 
  mutate(sex = ifelse(sex == 'F', "Female", "Male")) %>% 
  
  ggplot(aes(x = Axis.1, Axis.2)) +
  geom_point(aes(color = gm, shape = sex), size = 4) +
  scale_color_manual(values = c('red', 'dodgerblue'), name = 'GM',
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])))) +
  scale_shape_manual(values = c(17, 16), name = 'Sex') +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # labs(y = "USVs/min") +
  theme_prism() +
  labs(x = glue::glue('PCo1 - {round(p_var[1], 2)}%'), 
       y = glue::glue('PCo2 - {round(p_var[2], 2)}%'))+
  theme(
    legend.text = element_text(hjust = 0,
                               face = 'bold', 
                               color = 'black', 
                               size = 18),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 18),
    aspect.ratio = 1
  ) +
  guides(
    color = guide_legend(order = 1)
  ) 

ggsave('plots/USV/ctl/ctl_usv_calls_bc_pcoa.png',
       width = 5.5, height = 6, bg = 'white')


vegan::adonis2(usv_dist~gm*sex, data = pcoa_data, permutations = 9999) %>% 
  as_tibble(rownames = 'effect')

ctl_call_table <- table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>%
  mutate(rel_abund = n/sum(n),
         class = case_match(class, 
                            'chevron' ~ 'Chevron',
                            'complex' ~ 'Complex',
                            'down_fm' ~ 'Down Frequency Modulation',
                            'flat' ~ 'Flat',
                            'mult_steps' ~ 'Multiple Steps',
                            'rev_chevron' ~ 'Reverse Chevron',
                            'short' ~ 'Short',
                            'step_down' ~ 'Step Down',
                            'step_up' ~ 'Step Up',
                            'two_steps' ~ 'Two Steps',
                            'up_fm' ~ 'Up Frequency Modulation')) %>% 
  filter(gm %in% c('GM1', 'GM4')) 
 
  
ctl_call_table %>% 
  group_by(gm, sex, class) %>% 
  summarize(mean_rel_abund = mean(rel_abund)) %>% 
  ggplot(aes(x = sex, y = mean_rel_abund, fill = class)) +
  geom_col() +
  scale_fill_brewer(palette = 'Spectral', name = 'Call Class') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = scales::percent) +
  facet_wrap(~gm, scales = 'free') +
  labs(y = 'Mean Relative Abundance (%)') +
  theme(
    aspect.ratio = 3/1,
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 18),
    axis.text = element_text(face = 'bold', size = 18),
    legend.text = element_text(face = 'bold', size = 18),
    legend.title = element_text(face = 'bold', hjust = 0, size = 18),
  )

ggsave('plots/USV/ctl/call_composition.png', height = 5, width = 10)



ctl_call_table %>% 
  mutate(gm = case_match(gm,
                         'GM1' ~ 'GMLow',
                         'GM4' ~ 'GMHigh')) %>% 
  ungroup() %>% 
  group_by(class) %>% 
  rstatix::tukey_hsd(rel_abund ~ gm * sex) %>% 
  # write_tsv('stats/usv/ctl/call_class_anova_res.tsv')
  write_tsv('stats/usv/ctl/call_class_tukey_res.tsv')





table_metadata %>% 
  mutate(sex = case_when(sex == 'M' ~ 'Male',
                         sex == 'F' ~ 'Female')) %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>% 
  drop_na() %>% 
  summarize(total_usv = sum(n), .groups = 'drop') %>% 
  filter(gm %in% c('CF1', 'CF4')) %>% 
  ggplot(aes(x = gm, y = total_usv/5, shape = sex, color = gm)) +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(breaks = c('CF1', 'CF4'),
                   labels = c(expression(bold('CF'['Low'])),
                              expression(bold('CF'['High'])))) +
  labs(y = "Call Rate (USVs/min)") +
  theme_prism() +
  theme(
    axis.title.x = element_blank(),
    legend.text = element_text(face = 'bold', color = 'black', size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    legend.title = element_text(size = 16),
    
    aspect.ratio = 3/2
  ) +
  guides(
    color = 'none'
  )

ggsave(filename = 'plots/USV/cf/cf_usv_calls_per_min.png', width = 5, height = 4, bg = 'white')

table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>% 
  drop_na() %>% 
  summarize(total_usv = sum(n), .groups = 'drop') %>% 
  filter(gm %in% c('CF1', 'CF4')) %>% 
  mutate(usv_rate = total_usv/5) %>% 
  rstatix::anova_test(usv_rate ~ gm * sex) 

usv_dist <- table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  filter(gm %in% c('CF1', 'CF4')) %>%
  mutate(sampleid = paste(gm, sex, mouse, sep = '_')) %>% 
  group_by(mouse) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  mutate(n_rel_abund = (n/sum(n))*100) %>% 
  select(sampleid, class, n_rel_abund) %>% 
  pivot_wider(names_from = "class", values_from = 'n_rel_abund') %>% 
  column_to_rownames(var = 'sampleid') %>% 
  vegan::vegdist(method = 'bray')

usv_pcoa <- ape::pcoa(usv_dist, correction = 'cailliez')

metadata$mouse <- as.character(metadata$mouse)

pcoa_data <- usv_pcoa$vectors %>% 
  as_tibble(rownames = 'sampleid') %>% 
  separate(col = sampleid, into = c('gm', 'sex', 'mouse'), sep = '_') 

p_var <- (usv_pcoa$values$Eigenvalues/usv_pcoa$trace)*100

pcoa_data$gm <- factor(pcoa_data$gm, levels = c('GM1', 'GM4', 'CF1', 'CF4'))

pcoa_data %>% 
  mutate(sex = ifelse(sex == 'F', "Female", "Male")) %>% 
  
  ggplot(aes(x = Axis.1, Axis.2)) +
  geom_point(aes(color = gm, shape = sex), size = 4, stroke = 1.5) +
  scale_color_manual(values = c('red', 'dodgerblue'), name = 'GM',
                     labels = c(expression(bold('CF'['Low'])),
                                expression(bold('CF'['High'])))) +
  scale_shape_manual(values = c(2, 1), name = 'Sex') +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  # labs(y = "USVs/min") +
  theme_prism() +
  labs(x = glue::glue('PCo1 - {round(p_var[1], 2)}%'), 
       y = glue::glue('PCo2 - {round(p_var[2], 2)}%'))+
  theme(
    legend.text = element_text(hjust = 0,
                               face = 'bold', 
                               color = 'black', 
                               size = 18),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 18),
    aspect.ratio = 1
  ) +
  guides(
    color = guide_legend(order = 1)
  ) 

ggsave('plots/USV/cf/cf_usv_calls_bc_pcoa.png',
       width = 5.5, height = 4, bg = 'white')


vegan::adonis2(usv_dist~gm*sex, data = pcoa_data, permutations = 9999) %>% 
  as_tibble(rownames = 'effect') %>% 
  clipr::write_clip() %>% 
  select(effect, F, `Pr(>F)`) %>% 
  mutate(F = round(F, 4),
         `Pr(>F)` = round(`Pr(>F)`, 4))
  

table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>%
  mutate(rel_abund = n/sum(n),
         class = case_match(class, 
                            'chevron' ~ 'Chevron',
                            'complex' ~ 'Complex',
                            'down_fm' ~ 'Down Frequency Modulation',
                            'flat' ~ 'Flat',
                            'mult_steps' ~ 'Multiple Steps',
                            'rev_chevron' ~ 'Reverse Chevron',
                            'short' ~ 'Short',
                            'step_down' ~ 'Step Down',
                            'step_up' ~ 'Step Up',
                            'two_steps' ~ 'Two Steps',
                            'up_fm' ~ 'Up Frequency Modulation')) %>% 
  group_by(gm, sex, class) %>% 
  summarize(mean_rel_abund = mean(rel_abund)) %>% 
  filter(gm %in% c('CF1', 'CF4'))  %>% 
  ggplot(aes(x = sex, y = mean_rel_abund, fill = class)) +
  geom_col() +
  scale_fill_brewer(palette = 'Spectral', name = 'Call Class') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), labels = scales::percent) +
  facet_wrap(~gm, scales = 'free') +
  labs(y = 'Mean Relative Abundance (%)') +
  theme(
    aspect.ratio = 3/1,
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 'bold', size = 18),
    axis.text = element_text(face = 'bold', size = 18),
    legend.text = element_text(face = 'bold', size = 18),
    legend.title = element_text(face = 'bold', hjust = 0, size = 18),
  )

ggsave('plots/USV/cf/call_composition.png', height = 5, width = 10)


table_metadata %>% 
  filter(class != 'noise_dist') %>% 
  group_by(mouse, gm, sex) %>%
  mutate(rel_abund = n/sum(n),
         class = case_match(class, 
                            'chevron' ~ 'Chevron',
                            'complex' ~ 'Complex',
                            'down_fm' ~ 'Down Frequency Modulation',
                            'flat' ~ 'Flat',
                            'mult_steps' ~ 'Multiple Steps',
                            'rev_chevron' ~ 'Reverse Chevron',
                            'short' ~ 'Short',
                            'step_down' ~ 'Step Down',
                            'step_up' ~ 'Step Up',
                            'two_steps' ~ 'Two Steps',
                            'up_fm' ~ 'Up Frequency Modulation')) %>% 
  filter(gm %in% c('CF1', 'CF4')) %>% 
  mutate(gm = case_match(gm,
                         'CF1' ~ 'CFLow',
                         'CF4' ~ 'CFHigh')) %>% 
  ungroup() %>% 
  group_by(class) %>% 
  rstatix::anova_test(rel_abund ~ gm * sex) %>% 
  as_tibble() %>% 
  filter(p < 0.05)
  # write_tsv('stats/usv/cf/call_class_anova_res.tsv')
  write_tsv('stats/usv/cf/call_class_tukey_res.tsv')
  
  