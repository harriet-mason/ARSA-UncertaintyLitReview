library(tidyverse)
library(ggdist)

# Look at dists at X>27
my_map_data |>
  mutate(tdist = dist_normal(temp, variance)) |>
  ggplot(aes(y = county_name, xdist = tdist)) +
  stat_slab(aes(fill = after_stat(x > 27))) +
  facet_wrap(~variance_class)

# Alternative exceed map
exeed_data <- my_map_data |> 
  as_tibble() |>
  mutate(xprob = 1- pnorm(33, mean=temp, sd=variance)) 

# Low Variance
exeed_data |>
  filter(variance_class=="lowvar") |>
  ggplot() +
  geom_sf(aes(fill = xprob, 
              geometry = geometry), colour=NA) + 
  scale_fill_gradientn(colours = basecols, limits=c(0,1)) +
  #theme_void() +
  labs(fill = "P(X>27)") +
  theme_map() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.1, -0.2),
        legend.direction = "horizontal",
        legend.background = element_rect(fill=NA),
        plot.title = element_text(size = 14)) +   
  ggtitle("S1 - low uncertainty") 

# High Variance
exceed1 %+% filter(exeed_data, variance_class=="highvar") + 
  ggtitle("S2 - high uncertainty") 
