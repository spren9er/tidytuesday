library(tidyverse)
library(rgdal)
library(ggrepel)
library(ggforce)
library(cowplot)
library(magick)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-06-11/'
  )

meteorites <- read_csv(paste0(path, 'meteorites.csv'))

group_name <- Vectorize(function(name) {
  name <- str_replace_all(name, '\\d', '')
  name <- str_replace_all(name, '\\(.*\\)', '')
  name <- str_replace_all(name, '\\s\\w$', '')
  str_trim(name)
})

superclass <- Vectorize(function(class) {
  if (str_detect(class, '^H[\\d|\\~].*')) return('H')
  if (str_detect(class, '^LL\\d*.*')) return('LL')
  if (str_detect(class, '^L\\d.*')) return('L')
  if (str_starts(class, 'Iron')) return('Iron')
  class
})

format_kg <- Vectorize(function(mass) {
  mass_kg <- format(round(mass / 1000, 0), nsmall = 0)
  if (mass / 1000 < 100) mass_kg <- format(round(mass / 1000, 1), nsmall = 1)
  paste0(str_trim(mass_kg), 'kg')
})

grouped_meteorites <- meteorites %>%
  filter(
    !is.na(long), long >= -180, long <= 180,
     !is.na(lat),  lat >=  -90, lat  <=  90,
    long != 0, lat != 0
  ) %>%
  mutate(
    group_name = fct_lump(group_name(name), 50),
    superclass = fct_lump(superclass(class), 4)
  ) %>%
  filter(group_name != 'Other') %>%
  group_by(group_name) %>%
  mutate(group_size = n_distinct(geolocation)) %>%
  ungroup()

filtered_grouped_meteorites <- grouped_meteorites %>%
  group_by(group_name) %>%
  mutate(
    median_long = median(long),
    median_lat = median(lat)
  ) %>%
  filter(
    group_size >= 450,
    abs(long - median_long) <= 2 * IQR(long) | id %in% c(9594, 13504),
    abs(lat - median_lat) <= 2 * IQR(lat) | id %in% c(9594, 13504),
  ) %>%
  ungroup() %>%
  mutate(group_name = fct_reorder(group_name, group_size))

top_mass_meteorites <- filtered_grouped_meteorites %>%
  group_by(group_name) %>%
  top_n(3, mass) %>%
  arrange(desc(mass)) %>%
  mutate(
    rank = row_number(),
    label = paste0(
      rank, '. ', name, ' (',
      format_kg(mass),
      ')'
    )
  ) %>%
  ungroup()

groups <- filtered_grouped_meteorites %>%
  group_by(group_name) %>%
  summarize(
    median_long = first(median_long),
    median_lat = first(median_lat),
    group_size = first(group_size)
  )

# http://www.naturalearthdata.com/downloads/110m-physical-vectors/110m-land/
world <- readOGR(dsn = 'ne_110m_land', layer = 'ne_110m_land')
world_df <- fortify(world)

# http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/
countries <- readOGR(
  dsn = 'ne_110m_admin_0_countries', layer = 'ne_110m_admin_0_countries'
)
countries_df <- fortify(countries)

# %%
frames <- map(1:nrow(groups), function(idx) {
  group_name <- groups$group_name[idx]
  group_size <- groups$group_size[idx]
  group <- filter(groups, group_name == !!group_name)

  world_map <- ggplot() +
    geom_polygon(
      data = world_df,
      aes(x = long, y = lat, group = group, fill = hole)) +
    geom_path(
      data = countries_df,
      aes(x = long, y = lat, group = group, fill = hole),
      size = 0.1, color = '#cccccc'
    ) +
    geom_point(
      data = groups,
      aes(x = median_long, y = median_lat, size = group_size),
      alpha = 0.7, color = '#333333', show.legend = FALSE
    ) +
    geom_point(
      data = group,
      aes(x = median_long, y = median_lat, size = group_size),
      color = '#2dae81', show.legend = FALSE
    ) +
    scale_fill_manual(values = c('#dedede', 'transparent'), guide = 'none') +
    scale_size_continuous(range = c(0.01, 3)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_quickmap(clip = 'off') +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = 'transparent', color = 'transparent'
      ),
      plot.margin = margin(t = 0, b = 0, l = 0, r = 0)
    )

  meteorites_map <- ggplot() +
    geom_point(
      data = filter(filtered_grouped_meteorites, group_name == !!group_name),
      aes(x = long, y = lat, color = superclass, size = mass),
      alpha = 0.6
    ) +
    geom_label_repel(
      data = filter(top_mass_meteorites, group_name == !!group_name),
      aes(x = long, y = lat, color = superclass, label = label),
      size = 3, show.legend = FALSE
    ) +
    scale_size_continuous(
      range = c(0.2, 16),
      limits = c(0, 550001),
      breaks = c(1000, 10000, 25000, 100000, 250000),
      labels = c('1kg', '10kg', '25kg', '100kg', '250kg')
    ) +
    scale_color_viridis_d(drop = FALSE) +
    coord_cartesian() +
    theme_void() +
    labs(
      title = 'Areas with over 450 different meteorite locations',
      subtitle = paste0(
        idx, '. ',
        str_to_title(group_name), '  •  ', format(group_size, big.mark = ','),
        ' Meteorites'
      ),
      caption = '© 2019 spren9er  •  tidytuesday 24|2019',
      color = 'Meteorite Class',
      size = 'Meteorite Mass'
    ) +
    theme(
      text = element_text(family = 'Puritan'),
      panel.background = element_rect(fill = '#ffffff', color = 'transparent'),
      plot.title = element_text(
        margin = margin(b = 12), color = '#2dae81', size = 14, hjust = 0.5,
        face = 'bold'
      ),
      plot.subtitle = element_text(
        margin = margin(b = 15), size = 11, hjust = 0.5
      ),
      plot.caption = element_text(color = '#cccccc', size = 8, hjust = 0),
      plot.margin = margin(t = 0, b = 0),
      legend.title = element_text(size = 8, hjust = 0.5),
      legend.text = element_text(size = 7, color = '#777777', hjust = 1),
      legend.spacing.y = unit(10, 'points'),
      legend.margin = margin(b = 2, l = 20),
      legend.background = element_rect(fill = 'transparent', color = NA)
    )

  ggdraw() +
    draw_plot(
      meteorites_map, x = 0.05, y = 0.075, width = 0.9, height = 0.85
    ) +
    draw_plot(
      world_map, x = 0.68, y = 0.02, width = 0.3, height = 0.15, scale = 1
    ) +
    theme(plot.background = element_rect(fill = '#efefef', color = NA))

  filename <- paste0(
    'images/tidytuesday_201924_meteorites/tidytuesday_201924_meteorites',
    str_pad(idx, 2, pad = '0'),
    '.png'
  )

  ggsave(filename, width = 8, height = 8, dpi = 300)
  img <- image_read(filename)
  image_scale(img, '1200x1200')
})

animation <- image_animate(image_join(frames), fps = 0.4)

image_write(
  image = animation,
  path = 'images/tidytuesday_201924_meteorites.gif',
  quality = 100
)
