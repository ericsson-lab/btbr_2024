{
  library(tidyverse)
  library(gapminder)
  library(gganimate)
}

b6_position_data <- read_csv('data/2210_cohort/social_preference_test/221217_females/position_data/221217_females - Test 6 - TESTING.csv') 
btbr_position_data <- read_csv('data/2210_cohort/social_preference_test/221218_females/position_data/221218_females - Test 21 - TESTING.csv')

col_names <- c("time", "x", "y", "in_front", "in_middle", "in_back", "in_stranger", "in_object")

colnames(b6_position_data) <- col_names
colnames(btbr_position_data) <- col_names

b6_df <- b6_position_data %>% 
  mutate(pos = case_when(in_front == 1 ~ "Object",
                         in_middle == 1 ~ "Middle",
                         in_back == 1 ~ "Stranger"),
         step_number = 1:nrow(.),
         animal = 1) %>% 
  drop_na()

btbr_df <- btbr_position_data %>% 
  mutate(pos = case_when(in_front == 1 ~ "Object",
                         in_middle == 1 ~ "Middle",
                         in_back == 1 ~ "Stranger"),
         step_number = 1:nrow(.),
         animal = 1) %>% 
  drop_na()

b6_df$pos <- factor(b6_df$pos, levels = c("Stranger", "Middle", "Object"))
btbr_df$pos <- factor(btbr_df$pos, levels = c("Stranger", "Middle", "Object"))

# 
# arena_y <- b6_df %>%
#   drop_na() %>%
#   select(x, y, pos) %>%
#   group_by(pos) %>% 
#   summarize(y_start = max(y) ,
#             yend = y_start)
# 
# bottom <- data.frame("bottom", min(position_data_pos_named$y), min(position_data_pos_named$y))
# names(bottom) <- c("pos", "y_start", "yend")
# 
# arena <- rbind(arena_y, bottom) %>% 
#   mutate(xstart = min(position_data_pos_named$x),
#          xend = max(position_data_pos_named$x))


b6_plot <- b6_df %>% 
  drop_na() %>% 
  ggplot(aes(x = x, y = y, group = animal)) +
  # geom_segment(aes(x = xstart, xend = xend,
  #                  y = y_start, yend = yend),
  #              data = arena,
  #              inherit.aes = F,
  #              size = 1,
  #              color = "gray80") +
  # geom_segment(aes(x = min(xstart), xend = min(xstart),
  #                  y = max(y_start) + 1, yend = min(yend) - 1),
  #              data = arena,
  #              inherit.aes = F,
  #              size = 1,
  #              color = "gray80") +
  # geom_segment(aes(x = min(xend), xend = max(xend),
  #                  y = max(y_start) + 1, yend = min(yend)-1),
  #              data = arena,
  #              inherit.aes = F,
  #              size = 1,
  #              color = "gray80") +
  
  geom_path(aes(color = pos), linewidth = 0.75) +
  theme_void() +
  theme(legend.position = 'none') +
  scale_color_manual(values = c("red", "grey45", "dodgerblue")) +
  transition_reveal(time) +
  ease_aes()
  
animate(b6_plot,
        nframes = 700, 
        duration = 20, 
        renderer = gifski_renderer("plots/social_preference_test/b6_spt_test.gif", loop = F, width = 4, height = 5))



btbr_plot <- btbr_df %>% 
  drop_na() %>% 
  ggplot(aes(x = x, y = y, group = animal)) +geom_path(aes(color = pos), linewidth = 0.75) +
  theme_void() +
  theme(legend.position = 'none') +
  scale_color_manual(values = c("red", "grey45", "dodgerblue")) +
  transition_reveal(time) +
  ease_aes()



animate(btbr_plot,
        nframes = 700, 
        duration = 20, 
        renderer = gifski_renderer("plots/social_preference_test/btbr_spt_test.gif", loop = F))


            