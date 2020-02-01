library(tidyverse)
library(osmdata)
library(cowplot)
library(sf)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2020/2020-01-28/'
  )

data <- read_csv(paste0(path, 'sf_trees.csv'))

sf <- getbb('San Francisco, CA')

streets <- sf %>%
  opq() %>%
  add_osm_feature(
    key = 'highway',
    value = c('motorway', 'primary', 'secondary', 'tertiary')
  ) %>%
  osmdata_sf()

small_streets <- sf %>%
  opq() %>%
  add_osm_feature(
    key = 'highway',
    value = c(
      'residential', 'living_street', 'unclassified', 'service', 'footway'
    )
  ) %>%
  osmdata_sf()

bay_area <- st_read('data/ark28722-s7d02x-shapefile/s7d02x.shp')

p <- ggplot() +
  geom_sf(
    data = filter(bay_area, OBJECTID == 3),
    aes(geometry = geometry),
    size = 0.15,
    fill = '#393939',
    color = '#666666'
  ) +
  geom_sf(
    data = small_streets$osm_lines,
    inherit.aes = FALSE,
    color = '#cccccc',
    size = 0.025,
    alpha = 0.5
  ) +
  geom_sf(
    data = streets$osm_lines,
    inherit.aes = FALSE,
    color = '#cccccc',
    size = 0.05,
    alpha = 0.5
  ) +
  geom_point(
    data = data,
    aes(x = longitude, y = latitude),
    size = 0, alpha = 0.75, stroke = 0.5, color = '#266a2e', shape = 16
  ) +
  stat_density2d(
    data = data,
    aes(x = longitude, y = latitude, fill = stat(level), alpha = stat(level)),
    show.legend = FALSE, geom = 'polygon', n = 250, bins = 20
  ) +
  scale_fill_continuous(low = '#9eb89d', high = '#266a2e') +
  scale_alpha_continuous(range = c(0, 0.38)) +
  coord_sf(expand = FALSE) +
  xlim(x_coords) +
  ylim(y_coords) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = '#333333'),
    panel.background = element_rect(fill = '#333333')
  )

ggdraw() +
  draw_plot(p) +
  draw_label(
    'San Francisco Trees',
    x = 0.5, y = 0.885, fontfamily = 'Bitter', size = 35,
    color = '#cccccc', alpha = 0.45
  ) +
  draw_label(
    '#tidytuesday 05|2020',
    x = 0.5, y = 0.835, fontfamily = 'Bitter', size = 19,
    color = '#cccccc', alpha = 0.45
  ) +
  draw_label(
    '@spren9er',
    x = 0.838, y = 0.0775, fontfamily = 'Bitter', size = 10,
    color = '#cccccc', alpha = 0.2, angle = 83
  )

ggsave('images/tidytuesday_202005_san_francisco_trees.png', dpi = 89)
