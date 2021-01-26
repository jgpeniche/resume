# Libraries ====
options(tidyverse.quiet = T)
library(tidyverse)

# Graphical parameters ====
orange = '#FFAA02'
blues_pal <- RColorBrewer::brewer.pal(9, 'Blues') 

# Dateset ====
gen_dataf <- expand_grid(x = seq(1, 27, length.out = 27),
                         y = seq(1, 19, length.out = 19))

# Points and color ----
gen_dataf <- gen_dataf %>%
    mutate(groups = cut(x, seq(1, 27, length.out = 10),
                        include.lowest = T,
                        labels = as.character(seq(1, 9, by = 1))),
           normal_dist    = dnorm(x, mean = 20, sd = 0.5),
           max_bar_height = case_when(
               groups == 1 ~ 5, 
               groups == 2 ~ 7, 
               groups == 3 ~ 11, 
               groups == 4 ~ 15, 
               groups == 5 ~ 19, 
               groups == 6 ~ 13, 
               groups == 7 ~ 10, 
               groups == 8 ~ 6, 
               groups == 9 ~ 5),
           color = case_when(
               groups == 1 ~ blues_pal[2], 
               groups == 2 ~ blues_pal[3], 
               groups == 3 ~ blues_pal[4], 
               groups == 4 ~ blues_pal[5], 
               groups == 5 ~ blues_pal[6], 
               groups == 6 ~ blues_pal[5], 
               groups == 7 ~ blues_pal[4], 
               groups == 8 ~ blues_pal[3], 
               groups == 9 ~ orange)
    ) %>%
    group_by(groups) %>% 
    filter(y <= max_bar_height) %>% 
    ungroup()

# Final viz ====
orange_dens_dataf <- tibble(x = seq(8, 27, length.out = 100), y = dgamma(x, shape = 19.8, scale = 0.901))
blue_dens_dataf <- tibble(x = seq(0, 40, length.out = 100), y = dgamma(x, shape = 7.71, scale = 2))
final_graph <- gen_dataf %>% 
    ggplot(aes(x, y)) +
    geom_point(aes(color = groups), color = gen_dataf$color, size = 6) +
    geom_line(aes(x + 1, y*(22/max(orange_dens_dataf$y))), color = orange, size = 4, data = orange_dens_dataf) +
    geom_line(aes(x - 2, y * (7/max(blue_dens_dataf$y)) + 0.5), color = blues_pal[5], size = 4, data = blue_dens_dataf) +
    xlim(c(1, 27)) +
    theme_void() +
    theme(plot.background = element_rect(fil = '#000000'))+
    theme(legend.position = 'none')
final_graph + ggsave('figs/dists.jpeg')
