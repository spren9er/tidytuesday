library(tidyverse)
library(viridis)
library(gganimate)
library(sf)
library(rgdal)

path <-
  'https://raw.githubusercontent.com/tylerjrichards/Barstool_Pizza/master/'

pizza <- read_csv(paste0(path, 'pizza_data.csv')) %>%
  janitor::clean_names()

number_of_days <- nrow(pizza)

pizza.manhattan <- pizza %>%
  filter(
    city == 'New York',
    review_number != 128
  ) %>%
  mutate(
    score = review_stats_dave_average_score,
    day = review_number + 1
  ) %>%
  select(day, name, longitude, latitude, score) %>%
  drop_na() %>%
  arrange(day)

pizza.manhattan <- pizza.manhattan %>%
  mutate(
    next_longitude = lag(longitude),
    next_latitude = lag(latitude)
  )

days <- tibble(day = 1:number_of_days)
pizza.manhattan <- left_join(days, pizza.manhattan, by = 'day') %>%
  filter(!is.na(longitude) | !is.na(next_longitude)) %>%
  mutate(row_id = row_number())

nyc_sh <- readOGR(dsn='data/new_york')
nyc_sh@data$id = rownames(nyc_sh@data)
nyc_sh.points = fortify(nyc_sh, region='id')
nyc_df = inner_join(nyc_sh.points, nyc_sh@data, by='id')

animation <- ggplot(nyc_df, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), fill='#dedede') +
    geom_path(aes(group = group), color='#ffffff', size = 0.1) +
    geom_segment(
      data = pizza.manhattan,
      aes(
        x = longitude, y = latitude,
        xend = next_longitude, yend = next_latitude
      ),
      size = 0.2, color = '#333333'
    ) +
    geom_point(
      data = pizza.manhattan,
      aes(x = longitude, y = latitude, color = score),
      size = 1.1
    ) +
    geom_text(
      data = pizza.manhattan,
      aes(
        x = -73.98, y = 40.808,
        label = paste('Day', floor(day))
      ),
      color = "#333333", size = 2.75, hjust = 0.5, family = 'Barlow',
      fontface = 'bold'
    ) +
    geom_text(
      data = pizza.manhattan,
      aes(x = -73.98, y = 40.803, label = name),
      color = "#333333", size = 2.5, hjust = 0.5, family = 'Barlow'
    ) +
    coord_map() +
    xlim(c(-74.03, -73.93)) +
    ylim(c(40.7, 40.81)) +
    scale_color_viridis_c(limits = c(0, 10), breaks = seq(0,10, 2)) +
    transition_reveal(row_id, keep_last = FALSE) +
    shadow_wake(
      0.07, size = FALSE, alpha = TRUE, wrap = FALSE,
      falloff = 'sine-in', exclude_phase = 'enter', exclude_layer = c(1, 2, 5, 6)
    ) +
    labs(
      title = "Dave's Pizza Route through Manhattan",
      subtitle = '#tidytuesday 40|2019',
      caption = '© 2019 spren9er',
      color = "Rating"
    ) +
    theme_void() +
    theme(
      text = element_text(family = 'Barlow'),
      plot.title = element_text(
        margin = margin(t = 12), size = 11, face = 'plain'
      ),
      plot.subtitle = element_text(
        margin = margin(t = 6.5), size = 8, face = 'bold'
      ),
      plot.caption = element_text(
        color = '#dedede', size = 5, margin = margin(t = -12, b = 4),
        hjust = 0.94
      ),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7, margin = margin(b = 2)),
      legend.key.width = unit(0.35, 'cm')
    )

animate(
  animation, nframes = nrow(pizza.manhattan) * 2, width = 750, height = 900, res = 200
)

anim_save('images/tidytuesday_201940_all_the_pizza.gif')
