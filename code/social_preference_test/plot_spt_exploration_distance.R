{
  source('code/social_preference_test/load_spt_data.R')
}

plot_distance <- function(strain){
  df_exploration %>% 
    select(strain, gm, sex, distance) %>% 
    filter(strain %in% {{strain}}) %>% 
    ggplot(aes(x = gm, y = distance,  shape = sex, color = gm)) +
    geom_point( size = 3, position = position_dodge(0.5)) +
    geom_boxplot(color = "black", outlier.colour = NA, width = 0.5, fill = NA,
                 show.legend = F) +
    scale_y_continuous(expand = c(0,0),limits = c(20, 70)) +
    scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
    scale_shape_manual(values = c(16,17), name = "Sex") +
    theme_classic() +
    labs(y = "Distance (m)") +
    facet_wrap(~strain, strip.position = 'bottom') +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", color = "black", size = 14),
      axis.text = element_text(face = "bold", color = "black", size = 12),
      
      legend.text = element_text(face = "bold", color = "black", size = 12),
      legend.title = element_text(face = "bold", color = "black", 
                                  size = 12,hjust = 0.5),
      
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(face = "bold", color = "black", size = 14),
      strip.placement = 'outside',
      
      panel.grid = element_blank(),
      
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2)
    )
}

ggsave(plot = plot_distance(strain = c("BTBR", "B6")),
       file = 'plots/social_preference_test/distance_b6_btbr.png',
       width = 6, height = 4,
       units = c("in"),
       bg = 'white')
ggsave(plot = plot_distance(strain = c("BTBR")),
       file = 'plots/social_preference_test/distance_btbr.png',
       width = 4.5, height = 4,
       units = c("in"),
       bg = 'white')
ggsave(plot = plot_distance(strain = c("B6")) + ylim(c(20, 50)),
       file = 'plots/social_preference_test/distance_b6.png',
       width = 4.5, height = 4,
       units = c("in"),
       bg = 'white')

