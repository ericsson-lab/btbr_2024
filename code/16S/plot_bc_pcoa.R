{
  source('code/16S/data_beta_diversity.R')
  
  library(glue)
}

ctl_table <- table %>% 
  select(featureid, any_of(metadata %>% 
                             filter(group == 'CTL') %>% 
                             pull(sampleid)))

ctl_bc_dist <- generate_dist(table = ctl_table, distance = "bray")

ctl_bc_pcoa <- generate_pcoa(dist = ctl_bc_dist, metadata = metadata)

ctl_bc_pcoa[[1]] %>% 
  as.data.frame() %>% 
  ggplot(aes(x = PCo1, y = PCo2, 
             color = gm, shape = sex)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM",
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])))) +
  scale_shape_manual(name = "Sex", values = c(17,16),
                     labels = c('Female', 'Male')) +
  scale_x_continuous(limits = c(-.5, .5)) +
  labs(x = glue("PCo1 - ", round(ctl_bc_pcoa[[2]][1], 2), "%"),
       y = glue("PCo2 - ", round(ctl_bc_pcoa[[2]][2], 2), "%")) +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", face = "bold", size = 16),
    axis.title = element_text(color = "black", face = "bold", size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 18, hjust = 0),
    legend.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              order = 2),
         color = guide_legend(override.aes = list(size = 4),
                              order = 1))


ggsave('plots/16S/ctl/ctl_bc_pcoa.png',
       width = 5.5, height = 6)


ctl_dist <- ctl_table %>% 
  column_to_rownames(var = 'featureid') %>% 
  t() %>% 
  vegdist(., method = 'bray')

adonis2(formula = ctl_dist ~ gm * sex, 
        data = metadata %>% filter(group == 'CTL'), 
        permutations = 9999)




cf_table <- table %>% 
  select(featureid, any_of(metadata %>% 
                             filter(group == 'CF') %>% 
                             pull(sampleid)))

cf_bc_dist <- generate_dist(table = cf_table, distance = "bray")

cf_bc_pcoa <- generate_pcoa(dist = cf_bc_dist, metadata = metadata)

cf_bc_pcoa[[1]] %>% 
  as.data.frame() %>% 
  ggplot(aes(x = PCo1, y = PCo2, 
             color = gm, shape = sex)) +
  geom_point(size = 4, stroke = 1.5) +
  scale_color_manual(values = c("red", 'dodgerblue'), name = "GM",
                     labels = c(expression(bold('CF'['Low'])),
                                expression(bold('CF'['High'])))) +
  scale_shape_manual(name = "Sex", values = c(2, 1),
                     labels = c('Female', 'Male')) +
  scale_x_continuous(limits = c(-.5, .5)) +
  labs(x = glue("PCo1 - ", round(cf_bc_pcoa[[2]][1], 2), "%"),
       y = glue("PCo2 - ", round(cf_bc_pcoa[[2]][2], 2), "%")) +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", face = "bold", size = 16),
    axis.title = element_text(color = "black", face = "bold", size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 18, hjust = 0),
    legend.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
  ) +
  guides(shape = guide_legend(override.aes = list(size = 4),
                              order = 2, stroke = 1.5),
         color = guide_legend(order = 1))


ggsave('plots/16S/cf/cf_bc_pcoa.png',
       width = 5.5, height = 4)


cf_dist <- table %>% 
  select(featureid, any_of(metadata %>% 
                             filter(group == 'CF') %>% 
                             pull(sampleid))) %>% 
  column_to_rownames(var = 'featureid') %>% 
  t() %>% 
  vegdist(., method = 'bray')

adonis2(formula = cf_dist ~ gm * sex, 
        data = metadata %>% filter(group == 'CF'), 
        permutations = 9999)


cf_tabe <- table %>% 
  select(featureid, any_of(metadata %>% 
                             filter(GROUP == 'CF' & gm == 'CF1') %>% 
                             pull(sampleid)))

# j_dist <- generate_dist(distance = "jaccard")
cf_bc_dist <- generate_dist(table = cf_tabe, distance = "bray")

# j_pcoa <- generate_pcoa(j_dist)
cf_bc_pcoa <- generate_pcoa(cf_bc_dist)

cf_bc_pcoa[[1]] %>% 
  as.data.frame() %>% 
  ggplot(aes(x = PCo1, y = PCo2, 
             color = gm, shape = sex)) +
  geom_point(size = 4, stroke = 1.5) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM",
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])))) +
  scale_shape_manual(name = "Sex", values = c(2,1),
                     labels = c('Female', 'Male')) +
  labs(x = glue("PCo1 - ", round(cf_bc_pcoa[[2]][1], 2), "%"),
       y = glue("PCo2 - ", round(cf_bc_pcoa[[2]][2], 2), "%")) +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", face = "bold", size = 16),
    axis.title = element_text(color = "black", face = "bold", size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16, hjust = 0),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) 


ggsave('data/231011_CTL_CF/plots/cf_bc_pcoa.png',
       width = 7, height = 6)

metadata_full <- readxl::read_excel('data/231011_CTL_CF/data/230918_submission.xlsx',
                                    skip = 5)

cf_dist <- cf_tabe %>% 
  column_to_rownames(var = 'featureid') %>% 
  t() %>% 
  vegdist(., method = 'bray')

adonis2(formula = cf_dist ~ BIRTH_DAM, 
        data = metadata_full %>% filter(GROUP == 'CF' & gm == 'CF1'), 
        permutations = 9999)




ctl_cf_table <- table %>% 
  select(featureid, any_of(metadata %>% 
                             filter(GROUP %in%  c('CTL', 'CF')) %>% 
                             pull(sampleid)))

# j_dist <- generate_dist(distance = "jaccard")
ctl_cf_bc_dist <- generate_dist(table = ctl_cf_table, distance = "bray")

# j_pcoa <- generate_pcoa(j_dist)
ctl_cf_bc_pcoa <- generate_pcoa(ctl_cf_bc_dist)

ctl_cf_bc_pcoa[[1]] %>% 
  as.data.frame() %>% 
  ggplot(aes(x = PCo1, y = PCo2, 
             color = gm, shape = sex)) +
  geom_point(size = 4) + 
  scale_color_manual(values = c("red", "dodgerblue", "red", "dodgerblue"), name = "GM",
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])),
                                expression(bold('CF'['Low'])),
                                expression(bold('CF'['High'])))) +
  scale_shape_manual(name = "Sex", values = c(17,16),
                     labels = c('Female', 'Male')) +
  labs(x = glue("PCo1 - ", round(ctl_cf_bc_pcoa[[2]][1], 2), "%"),
       y = glue("PCo2 - ", round(ctl_cf_bc_pcoa[[2]][2], 2), "%")) +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", face = "bold", size = 16),
    axis.title = element_text(color = "black", face = "bold", size = 16),
    legend.text = element_text(color = "black", face = "bold", size = 16, hjust = 0),
    legend.title = element_text(color = "black", face = "bold", size = 16, hjust = 0.5),
  ) +
  guides(shape = guide_legend(override.aes = list(size = 3),
                              order = 2),
         color = guide_legend(override.aes = list(size = 3),
                              order = 1))


ggsave('data/231011_CTL_CF/plots/ctl_bc_pcoa.png',
       width = 6, height = 5)
ctl_cf_dist <- ctl_cf_table %>% 
  column_to_rownames(var = 'featureid') %>% 
  t() %>% 
  vegdist(., method = 'bray')

adonis2(formula = ctl_cf_dist ~ GROUP, 
        data = metadata %>% filter(GROUP %in% c('CTL', 'CF')), 
        permutations = 9999)

