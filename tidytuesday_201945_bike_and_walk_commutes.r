library(tidyverse)
library(geofacet)
library(xkcd)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-11-05/'
  )

data <- read_csv(paste0(path, 'commute.csv'))

totals <- data %>%
  mutate(
    state_abb = ifelse(state == 'District of Columbia', 'DC', state_abb)
  ) %>%
  drop_na(state_abb) %>%
  group_by(mode, state, state_abb, city_size) %>%
  summarize(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    mode = str_to_lower(mode),
    city_size = str_to_lower(city_size)
  )

percentages <- totals %>%
  group_by(state, state_abb, city_size) %>%
  mutate(total_city_size = sum(total)) %>%
  group_by(state, state_abb) %>%
  mutate(
    total_state = sum(total),
    percentage_city_size = total_city_size / total_state,
    percentage = total / total_city_size
  ) %>%
 pivot_wider(names_from = mode, values_from = c(percentage, total)) %>%
 select(
   state, state_abb, city_size, percentage_city_size, percentage_walk,
   percentage_bike
 ) %>%
 arrange(state, state_abb, city_size)

cum_percentages <- percentages %>%
  group_by(state, state_abb) %>%
  mutate(
    cum_percentage_city_size = cumsum(percentage_city_size),
    lag_cum_percentage_city_size = lag(cum_percentage_city_size, default = 0)
  )

cum_percentages %>%
  ggplot() +
  xkcdrect(
    data = cum_percentages,
    aes(
      xmin = 0, xmax = percentage_walk,
      ymin = lag_cum_percentage_city_size, ymax = cum_percentage_city_size,
      fill = paste('walk', city_size, sep = ' / ')
    ), size = 0.1, show.legend = FALSE) +
  xkcdrect(
    data = cum_percentages,
    aes(
      xmin = percentage_walk, xmax = 1,
      ymin = lag_cum_percentage_city_size, ymax = cum_percentage_city_size,
      fill = paste('bike', city_size, sep = ' / ')
    ), size = 0.1, show.legend = FALSE) +
  scale_fill_manual(
    values = c(
      '#161A29', '#11433F', '#217D66', '#7E4997', '#726BA4', '#85A6CD'
    )
  ) +
  coord_fixed() +
  facet_geo(~state_abb) +
  theme_void() +
  theme(
    text = element_text(family = 'xkcd Script'),
    strip.background = element_blank(),
    plot.title = element_text(
      margin = margin(t = 15), size = 15, hjust = 0.5
    ),
    plot.subtitle = element_text(
      margin = margin(t = 10, b = 11), size = 11, hjust = 0.5
    ),
    plot.caption = element_text(
      color = '#dedede', size = 7, margin = margin(t = 5, b = 6),
      hjust = 0.995
    )
  ) +
  labs(
    title = 'Walk/Bike Ratios of Commutes in U.S. 2008 - 2012',
    subtitle = '(per State & City Size) #tidytuesday 45/2019',
    caption = '© 2019 spren9er'
  )

ggsave('images/tidytuesday_201945_bike_and_walk_commutes.png', dpi = 150)
