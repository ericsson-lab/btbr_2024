{
  source('code/social_preference_test/load_spt_data.R')
}

  df_testing %>% 
    filter(strain== 'BTBR') %>% 
    select(strain, gm, sex, spi) %>% 
    ggplot(aes(x = gm, y = spi, shape = sex)) +
    geom_hline(aes(yintercept = 0), linetype = 2, color = "black",
               alpha = 0.8, linewidth = 1.5) +
    geom_point(aes(color = gm,), size = 4, 
               position = position_jitterdodge(dodge.width =  0.6, 
                                               jitter.width = 0.05)) +
    geom_boxplot(color = "black", outlier.colour = NA, 
                 width = 0.5, fill = NA,
                 show.legend = F, linewidth = 1.5, position = position_dodge(0.6)) +
    scale_y_continuous(limits = c(-0.83, 0.83), 
                       breaks = c(0.8, 0.4, 0, -0.4, -0.8),
                       expand = c(0,0)) +
    scale_x_discrete(breaks = c('GM1', 'GM4'),
                     labels = c(expression(bold('GM'['Low'])),
                                expression(bold('GM'['High'])))) +
    scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
    scale_shape_manual(values = c(17, 16), name = "Sex") +
    ggprism::theme_prism() +
    labs(y = "Social Preference Index") +

    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = "bold", color = "black", size = 18),
      axis.text = element_text(face = "bold", color = "black", size = 18),
      
      legend.text = element_text(face = "bold", color = "black", size = 18),
      legend.title = element_text(face = "bold", color = "black", 
                                  size = 18,hjust = 0.5),
      
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(face = "bold", color = "black", size = 18),
      strip.placement = 'outside',
      
      panel.grid = element_blank(),
      aspect.ratio = 3/2
    ) +
    guides(
      color = 'none',
      shape = guide_legend(order = 2)
    )

  
  df_testing %>% 
    filter(strain== 'BTBR') %>% 
    select(strain, gm, sex, spi) %>% 
    anova_test(spi ~ gm * sex)
  
  
  
  
ggsave('plots/spt/ctl/spi_btbr.png',
       width = 6, height = 5, bg = 'white')

df_testing %>% 
  filter(strain== 'B6') %>% 
  select(strain, gm, sex, spi) %>% 
  ggplot(aes(x = gm, y = spi, shape = sex)) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "black",
             alpha = 0.8, linewidth = 1.5) +
  geom_point(aes(color = gm,), size = 4, 
             position = position_jitterdodge(dodge.width =  0.6, 
                                             jitter.width = 0.05)) +
  geom_boxplot(color = "black", outlier.colour = NA, 
               width = 0.5, fill = NA,
               show.legend = F, linewidth = 1.5, position = position_dodge(0.6)) +
  scale_y_continuous(limits = c(-0.83, 0.83), 
                     breaks = c(0.8, 0.4, 0, -0.4, -0.8),
                     expand = c(0,0)) +
  scale_x_discrete(breaks = c('GM1', 'GM4'),
                   labels = c(expression(bold('GM'['Low'])),
                              expression(bold('GM'['High'])))) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(17, 16), name = "Sex") +
  ggprism::theme_prism() +
  labs(y = "Social Preference Index") +
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 18),
    axis.text = element_text(face = "bold", color = "black", size = 18),
    
    legend.text = element_text(face = "bold", color = "black", size = 18),
    legend.title = element_text(face = "bold", color = "black", 
                                size = 18,hjust = 0.5),
    
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(face = "bold", color = "black", size = 18),
    strip.placement = 'outside',
    
    panel.grid = element_blank(),
    aspect.ratio = 3/2
  ) +
  guides(
    color = 'none',
    shape = guide_legend(order = 2)
  )


ggsave('plots/spt/b6/spi_b6.png',
       width = 6, height = 5, bg = 'white')

df_testing %>% 
  filter(strain== 'B6') %>% 
  select(strain, gm, sex, spi) %>% 
  anova_test(spi ~ gm * sex)




cf_df_testing %>% 
  filter(strain== 'BTBR') %>% 
  select(strain, gm, sex, spi) %>% 
  ggplot(aes(x = gm, y = spi, shape = sex)) +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "black",
             alpha = 0.8, linewidth = 1.5) +
  geom_point(aes(color = gm,), size = 4, stroke = 1.5, 
             position = position_jitterdodge(dodge.width =  0.6, 
                                             jitter.width = 0.05)) +
  geom_boxplot(color = "black", outlier.colour = NA, 
               width = 0.5, fill = NA,
               show.legend = F, linewidth = 1.5, position = position_dodge(0.6)) +
  scale_y_continuous(limits = c(-1, 1), 
                     breaks = c(1, 0.5, 0, -0.5, -1),
                     expand = c(0.01, 0.01)) +
  scale_x_discrete(breaks = c('CF1', 'CF4'),
                   labels = c(expression(bold('CF'['Low'])),
                              expression(bold('CF'['High'])))) +
  scale_color_manual(values = c("red", "dodgerblue"), name = "GM") +
  scale_shape_manual(values = c(2, 1), name = "Sex") +
  ggprism::theme_prism() +
  labs(y = "Social Preference Index") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 20),
    axis.text = element_text(face = "bold", color = "black", size = 20),
    
    legend.text = element_text(face = "bold", color = "black", size = 20),
    legend.title = element_text(face = "bold", color = "black", 
                                size = 18,hjust = 0.5),
    
    aspect.ratio = 3/2
  ) +
  guides(
    color = 'none',
    shape = guide_legend(order = 2)
  )

cf_df_testing %>% 
  filter(strain== 'BTBR') %>% 
  select(strain, gm, sex, spi) %>% 
  anova_test(spi ~ gm * sex)


ggsave('plots/spt/cf/spi_btbr.png',
       width = 6, height = 5, bg = 'white')
