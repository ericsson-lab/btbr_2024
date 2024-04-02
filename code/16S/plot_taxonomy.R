source('code/16S/data_beta_diversity.R')
source('code/16S/data_alpha_diversity_metrics.R')
library(usedist)
library(ggtree)
library(ggnewscale)
library(RColorBrewer)
library(patchwork)
library(gridExtra)


ctl_mice <- metadata %>% 
  filter(group == 'CTL') 

bc_dist <- generate_dist(table = table, distance = 'bray')

ctl_bc_dist <- bc_dist %>% 
  dist_subset(., ctl_mice$sampleid)

clust_res <- hclust(ctl_bc_dist)

tree <- ggtree(clust_res, size = 1, ) 
tree + geom_tiplab() +
  xlim(0, 1)

gm <- ctl_mice %>% 
  select(sampleid, gm) %>% 
  column_to_rownames(var = 'sampleid')

sex <- ctl_mice %>% 
  select(sampleid, sex) %>% 
  column_to_rownames(var = 'sampleid')

chao1 <- alpha_stats %>% 
  filter(sampleid %in% ctl_mice$sampleid) %>% 
  select(sampleid, chao1) %>% 
  column_to_rownames(var = 'sampleid')

p1 <- gheatmap(tree, gm, width = 0.1, colnames = F, color = NA,
               custom_column_labels = c('GM'),
               colnames_angle = 90) +
  scale_fill_manual(values = c('red', 'dodgerblue'), name = 'GM',
                    labels = c(expression(bold('GM'['Low'])),
                               expression(bold('GM'['High']))))  +
theme(aspect.ratio = 2/1)

p2 <- p1 + new_scale_fill()
p3 <- gheatmap(p2, sex, width = 0.1, colnames = F, color = NA,
               offset=0.05,colnames_angle = 90) +
  scale_fill_manual(values = c('black', 'darkgray'), name = 'Sex',
                    labels = c('Female',
                               'Male') ) +
  theme(
    aspect.ratio = 3/1,
    legend.text = element_text(face = 'bold', size = 18, hjust = 0),
    legend.title = element_text(face = 'bold', size = 18, hjust = 0)
  ) 

f_names <- taxonomy %>% 
  pivot_wider(names_from = 'level', values_from = 'taxon') %>% 
  mutate(Family = paste(Phylum, Family, sep = "_")) %>% 
  select(featureid, Family)

ctl_mice_table <- table %>% 
  select(featureid, all_of(ctl_mice$sampleid))
other_list <- ctl_mice_table %>% 
  pivot_longer(-featureid, names_to = 'sampleid', values_to = 'count') %>% 
  left_join(., f_names) %>% 
  group_by(sampleid, Family) %>% 
  summarise(count = sum(count), .groups = 'drop') %>% 
  group_by(sampleid) %>% 
  mutate(rel_abund = count/sum(count)) %>% 
  group_by(Family) %>% 
  summarise(mean = mean(rel_abund)) %>% 
  mutate(pass = case_when(mean < 0.005 ~ F,
                          TRUE ~ T)) %>% 
  select(Family, pass) %>% 
  distinct()

data_to_plot <- ctl_mice_table %>% 
  pivot_longer(-featureid, names_to = 'sampleid', values_to = 'count') %>% 
  left_join(., f_names) %>%
  left_join(., other_list) %>% 
  mutate(Family = case_when(pass == TRUE ~ Family, 
                            pass == FALSE ~ 'Other_Other')) %>% 
  separate(Family, into = c('Phylum', 'Family'), extra = 'merge') %>% 
  group_by(sampleid, Family) %>% 
  summarise(count = sum(count), .groups = 'drop') %>% 
  group_by(sampleid) %>% 
  mutate(rel_abund = count/sum(count)) %>% 
  ungroup()


family_order <- data_to_plot %>% 
  ungroup() %>% 
  select(Family) %>% 
  distinct() %>% 
  filter(Family != 'Other') %>% 
  arrange(Family) %>% 
  rbind(., 'Other') %>% 
  pull(Family)

palette <- c(colorRampPalette(brewer.pal(11, "Spectral"))(length(family_order)-1), '#606060')

sample_order <- get_taxa_name(tree)

rel_abund_plot <- data_to_plot %>% 
  mutate(sampleid = factor(sampleid, levels = rev(sample_order))) %>% 
  ggplot(aes(x = sampleid, 
             y = rel_abund, fill = factor(Family, levels = c(family_order))), 
         color = 'white') +
  geom_col(width = 0.9) +
  ylab('Relative Abundance') +
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0, 1),
                     labels = scales::percent) +
  scale_fill_manual(values = c(palette), name = 'Family') +
  ggprism::theme_prism() +
  coord_flip() +
  theme(
    # aspect.ratio = 1/2,
    axis.text.y = element_blank(),
    axis.title.y= element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(face = 'bold', size = 18),
    legend.title = element_text(face = 'bold', size = 18, hjust = 0)
    
  )

data_to_plot %>% 
  left_join(., metadata) %>% 
  filter(Family == 'Deferribacteraceae') %>% 
  group_by(gm) %>% 
  summarise(mean = mean(rel_abund))
p4 <- p3 + theme(
  plot.margin = margin(0,0,0,0)
)
  
p4 +rel_abund_plot + 
  plot_layout(guides = 'collect')

ggsave('plots/16S/ctl/ctl_taxonomy.png', width = 10, height = 8)



cf_mice <- metadata %>% 
  filter(group == 'CF' | sampleid %in% c('ZM133', 'ZM134',
                                         'ZM135','ZM136'))

cf_bc_dist <- bc_dist %>% 
  dist_subset(., cf_mice$sampleid)

clust_res <- hclust(cf_bc_dist)

tree <- ggtree(clust_res, size = 1, ) 
tree_flipped <- flip(tree, 69, 70)

tree + geom_tiplab() + xlim(0,1)
tree_flipped + geom_tiplab() + xlim(0,1)

gm <- cf_mice %>% 
  select(sampleid, gm) %>% 
  column_to_rownames(var = 'sampleid')

sex <- cf_mice %>% 
  select(sampleid, sex) %>% 
  column_to_rownames(var = 'sampleid')

p1 <- gheatmap(tree_flipped, gm, width = 0.1, colnames = F, color = NA,
               custom_column_labels = c('GM'),
               colnames_angle = 90) +
  scale_fill_manual(values = c('#ff8080', '#8ec7ff', 'red', 'dodgerblue'), name = 'GM',
                    labels = c(expression(bold('CF'['Low'])),
                               expression(bold('CF'['High'])),
                               expression(bold('GM'['Low'])),
                               expression(bold('GM'['High']))))  +
  theme(aspect.ratio = 2/1)

p2 <- p1 + new_scale_fill()
p3 <- gheatmap(p2, sex, width = 0.1, colnames = F, color = NA,
               offset=0.05,colnames_angle = 90) +
  scale_fill_manual(values = c('white','black', 'darkgray'), name = 'GM',
                    labels = c('Dam', 
                               'Female',
                               'Male') ) +
  theme(
    aspect.ratio = 3/1,
    legend.text = element_text(face = 'bold', size = 18, hjust = 0),
    legend.title = element_text(face = 'bold', size = 18, hjust = 0)
  ) 

sample_order <- get_taxa_name(p3)

f_names <- taxonomy %>% 
  pivot_wider(names_from = 'level', values_from = 'taxon') %>% 
  mutate(Family = paste(Phylum, Family, sep = "_")) %>% 
  select(featureid, Family)

cf_mice_table <- table %>% 
  select(featureid, all_of(cf_mice$sampleid))

other_list <- cf_mice_table %>% 
  pivot_longer(-featureid, names_to = 'sampleid', values_to = 'count') %>% 
  left_join(., f_names) %>% 
  group_by(sampleid, Family) %>% 
  summarise(count = sum(count), .groups = 'drop') %>% 
  group_by(sampleid) %>% 
  mutate(rel_abund = count/sum(count)) %>% 
  group_by(Family) %>% 
  summarise(mean = mean(rel_abund)) %>% 
  mutate(pass = case_when(mean < 0.005 ~ F,
                          TRUE ~ T)) %>% 
  select(Family, pass) %>% 
  distinct() 

data_to_plot <- cf_mice_table %>% 
  pivot_longer(-featureid, names_to = 'sampleid', values_to = 'count') %>% 
  left_join(., f_names) %>%
  left_join(., other_list) %>% 
  mutate(Family = case_when(pass == TRUE ~ Family, 
                            pass == FALSE ~ 'Other_Other')) %>% 
  separate(Family, into = c('Phylum', 'Family'), extra = 'merge') %>% 
  group_by(sampleid, Family) %>% 
  summarise(count = sum(count), .groups = 'drop') %>% 
  group_by(sampleid) %>% 
  mutate(rel_abund = count/sum(count)) %>% 
  ungroup()

family_order <- data_to_plot %>% 
  ungroup() %>% 
  select(Family) %>% 
  distinct() %>% 
  filter(Family != 'Other') %>% 
  arrange(Family) %>% 
  rbind(., 'Other') %>% 
  pull(Family)

palette <- c(colorRampPalette(brewer.pal(11, "Spectral"))(length(family_order)-1), '#606060')

rel_abund_plot <- data_to_plot %>% 
  mutate(sampleid = factor(sampleid, levels = rev(sample_order))) %>% 
  ggplot(aes(x = sampleid, 
             y = rel_abund, fill = factor(Family, levels = c(family_order))), 
         color = 'white') +
  geom_col(width = 0.9) +
  ylab('Relative Abundance') +
  scale_y_continuous(expand = expansion(mult = c(0,0)),
                     limits = c(0, 1),
                     labels = scales::percent) +
  scale_fill_manual(values = c(palette), name = 'Family') +
  ggprism::theme_prism() +
  coord_flip() +
  theme(
    # aspect.ratio = 1/2,
    axis.text.y = element_blank(),
    axis.title.y= element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(face = 'bold', size = 18),
    legend.title = element_text(face = 'bold', size = 18, hjust = 0)
    
  )

p3 + rel_abund_plot +
  plot_layout(guides = 'collect')

ggsave('plots/16S/cf/cf_taxonomy.png', width = 10, height = 8)



data_to_plot %>% 
  left_join(., metadata) %>% 
  filter(Family == 'Acholeplasmataceae') %>% 
  group_by(gm) %>% 
  rstatix::get_summary_stats(rel_abund)
