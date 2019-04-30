library(tidyverse)
library(treemapify)
library(lubridate)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-30/'
  )

bird_collisions <- read_csv(paste0(path, 'bird_collisions.csv'))
mp_light <- read_csv(paste0(path, 'mp_light.csv'))

mp_bird_collisions <- bird_collisions %>%
  filter(locality == 'MP', year(date) >= 2000) %>%
  left_join(mp_light, by = 'date')

mp_bird_collisions.agg <- mp_bird_collisions %>%
  mutate(species = str_to_title(species)) %>%
  group_by(family, genus, species) %>%
  summarize(
    deaths = n(),
    avg_light_score = mean(light_score, na.rm = TRUE)
  )

mp_bird_collisions.agg %>%
  ggplot(aes(
    area = deaths, fill = avg_light_score,
    subgroup = family, subgroup2 = genus, label = species
  )) +
    geom_treemap(size = 0) +
    geom_treemap_text(place = 'middle', size = 8, min.size = 0) +
    geom_treemap_subgroup2_border(color = '#ffffff', size = 1) +
    geom_treemap_subgroup2_text(
      color = '#ffffff', place = 'topleft', size = 10, min.size = 8
    ) +
    geom_treemap_subgroup_border(color = '#430252', size = 1) +
    geom_treemap_subgroup_text(
      color = '#430252', place = 'center', size = 18, min.size = 3
    ) +
    scale_fill_viridis_c(option = 'magma') +
    labs(
      title = 'Chicago Bird Collisions (McCormick Place) 2000-2016',
      subtitle = expression(
        atop('#tidytuesday 18|2019', atop('[size of tile ≃ number of deaths]'))
      ),
      fill = 'Avg. Light Score',
      caption = '© 2019 spren9er'
    )

ggsave(
  'images/tidytuesday_201918_chicago_bird_collisions.png',
  width = 7, height = 7, bg = 'transparent'
)
