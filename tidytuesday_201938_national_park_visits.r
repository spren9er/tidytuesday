library(tidyverse)
library(gganimate)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-09-17/'
  )

parks <- read_csv(paste0(path, 'national_parks.csv'))
parks <- parks %>%
  mutate(
    parkname =
      if_else(
        is.na(parkname),
        str_trim(str_remove(unit_name, 'National Park')),
        parkname
      )
  )

highlight_parks <- c(
  'GREAT SMOKY MOUNTAINS',
  'GRAND CANYON',
  'ROCKY MOUNTAIN',
  'YOSEMITE',
  'YELLOWSTONE',
  'ZION',
  'ACADIA',
  'DENALI',
  'HOT SPRINGS',
  'CARLSBAD CAVERNS',
  'GREAT BASIN'
)

highlight_colors <- c(
  '#223e15',
  '#176785',
  '#499989',
  '#5fa73f',
  '#ff8706',
  '#ff534e',
  '#f5b901',
  '#9a91fa',
  '#c988d2',
  '#6da5c2',
  '#fe43bc'
)

ranking_parks <- parks %>%
  filter(
    year != 'Total',
    unit_type == 'National Park',
    !is.na(parkname),
    unit_name != 'Denali National Preserve'
  ) %>%
  mutate(
    year = as.integer(year),
    parkname = str_to_upper(parkname)
  ) %>%
  filter(year < 2016) %>%
  group_by(year) %>%
  arrange(year, desc(visitors)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top_parks <- ranking_parks %>%
  filter(parkname %in% highlight_parks) %>%
  mutate(parkname = fct_relevel(str_to_upper(parkname), highlight_parks)) %>%
  arrange(year, desc(parkname))

other_parks <- ranking_parks %>%
  filter(!parkname %in% highlight_parks)

animation <- top_parks %>%
  ggplot(aes(x = year, y = rank, group = parkname, color = parkname)) +
  geom_line(
    data = other_parks, size = 0.5, show.legend = FALSE, color = '#dadada'
  ) +
  geom_line(show.legend = FALSE, size = 0.8) +
  geom_text(
    aes(x = year + 0.8, label = parkname),
    size = 4.5, show.legend = FALSE, hjust = 0, fontface = 'bold'
  ) +
  scale_x_continuous(breaks = c(1925, 1950, 1975, 2000)) +
  scale_y_continuous(
    breaks = c(1, 25, 50), labels = c('1ˢᵗ', '25ᵗʰ', '50ᵗʰ'), trans = 'reverse'
  ) +
  scale_color_manual(values = highlight_colors) +
  coord_cartesian(clip = 'off') +
  transition_reveal(year, keep_last = TRUE) +
  labs(
    title = 'The most popular national parks',
    subtitle = 'National parks ranked by number of visitors in a given year',
    x = '',
    y = 'Rank',
    caption = '#tidytuesday 38|2019  •  © 2019 spren9er'
  ) +
  theme(
    plot.background = element_rect(fill = '#f0f0f0'),
    plot.margin = margin(t = 40, r = 155, b = 20, l = 20),
    plot.title = element_text(
      margin = margin(b = 8), size = 38, hjust = -0.17, face = 'bold',
      color = '#333333'
    ),
    plot.subtitle = element_text(
      margin = margin(t = 6, b = 5), size = 29, hjust = -0.72,
      face = 'plain', color = '#333333'
    ),
    plot.caption = element_text(
      color = '#999999', size = 13, margin = margin(t = 10), hjust = 0.5,
      face = 'plain', family = 'Decima Mono Pro'
    ),
    panel.background = element_rect(fill = '#f0f0f0'),
    panel.grid.major = element_line(size = 0.5, color = '#d3d3d3'),
    panel.border = element_blank(),
    axis.text.x = element_text(
      family = 'Decima Mono Pro', color = '#999999', face = 'plain', size = 20,
      margin = margin(t = 6)
    ),
    axis.text.y = element_text(
      family = 'Decima Mono Pro', color = '#999999', face = 'plain', size = 20,
      margin = margin(r = 6)
    ),
    axis.title.y = element_text(color = '#333333', face = 'bold', size = 16)
  )

animate(animation, width = 1000, height = 1000, end_pause = 30)

anim_save('images/tidytuesday_201938_national_park_visits.gif')
