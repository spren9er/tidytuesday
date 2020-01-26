library(tidyverse)
library(ggforce)
library(cowplot)

data <- tidytuesdayR::tt_load(2020, week = 4)

tracks <- data$spotify_songs %>%
  distinct(track_id, .keep_all = TRUE)

parse_genre <- Vectorize(function(genre) {
  genre <- str_to_title(genre)
  ifelse(genre == 'Edm', 'EDM', genre)
})

parse_subgenre <- Vectorize(function(subgenre) {
  subgenre <- str_to_title(subgenre)
  case_when(
    subgenre == 'Progressive Electro House' ~ 'Progr. Electro House',
    subgenre == 'Urban Contemporary' ~ 'Urban Contemp.',
    subgenre == 'Pop Edm' ~ 'Pop EDM',
    TRUE ~ subgenre
  )
})

features <- tracks %>%
  mutate(
    genre = parse_genre(playlist_genre),
    subgenre = parse_subgenre(playlist_subgenre)
  ) %>%
  select(genre, subgenre, energy, loudness, speechiness, valence, tempo)

no_features <- features %>%
  colnames() %>%
  length() - 2

alpha <- 2 * pi / no_features
shift <- 0.2
offset_y <- 0.05

genre_features <- features %>%
  select(-subgenre) %>%
  group_by(genre) %>%
  summarize_all(mean) %>%
  mutate_at(vars(-genre), rescale) %>%
  pivot_longer(-genre, names_to = 'feature', values_to = 'value') %>%
  arrange(genre, feature) %>%
  group_by(genre) %>%
  mutate(no = row_number()) %>%
  ungroup()

subgenre_features <- features %>%
  select(-genre) %>%
  group_by(subgenre) %>%
  summarize_all(mean) %>%
  mutate_at(vars(-subgenre), rescale) %>%
  pivot_longer(-subgenre, names_to = 'feature', values_to = 'value') %>%
  arrange(subgenre, feature) %>%
  group_by(subgenre) %>%
  mutate(no = row_number()) %>%
  rename(genre = subgenre) %>%
  ungroup()

from_polar <- function(radius, angle) {
  radius * c(cos(angle + pi / 2), sin(angle + pi / 2))
}

circle_for <- function(no, alpha, value, shift) {
  angle <- (no - 1) * alpha + alpha / 2
  radius <- value / cos(alpha / 2)

  circle_center <- from_polar(radius, angle) + from_polar(shift, angle)
  circle_radius <- value * tan(alpha / 2)

  result <- c(circle_center, circle_radius)
  names(result) <- c('center_x', 'center_y', 'radius')

  result
}

frame_for_genre <- function(genre) {
  genre_frame <- filter(genre_features, genre == !!genre)
  if (nrow(genre_frame) > 0) {
    return(genre_frame)
  }

  filter(subgenre_features, genre == !!genre)
}

triangles_for <- function(genre, type) {
  genre_frame <- frame_for_genre(genre)

  if (type == 'full') genre_frame <- mutate(genre_frame, value = 1)

  genre_frame %>%
    filter(value > 0) %>%
    rowwise() %>%
    mutate(
      shift_x = from_polar(shift, (no - 1) * alpha + alpha / 2)[1],
      shift_y = from_polar(shift, (no - 1) * alpha + alpha / 2)[2],
      x1 = from_polar(value, (no - 1) * alpha)[1] + shift_x,
      y1 = from_polar(value, (no - 1) * alpha)[2] + shift_y,
      x2 = from_polar(value, no * alpha)[1] + shift_x,
      y2 = from_polar(value, no * alpha)[2] + shift_y,
      x3 = shift_x,
      y3 = shift_y
    ) %>%
    pivot_longer(
      cols = x1:y3,
      names_to = c('coord', 'id'),
      names_pattern = '(\\w)(\\d)',
      values_to = 'temp_value'
    ) %>%
    pivot_wider(
      names_from = coord,
      values_from = temp_value
    ) %>%
    mutate(type = type)
}

circles_for <- function(genre, type) {
  genre_frame <- frame_for_genre(genre)

  if (type == 'full') genre_frame <- mutate(genre_frame, value = 1)

  genre_frame %>%
    filter(value > 0) %>%
    rowwise() %>%
    mutate(
      center_x = circle_for(no, alpha, value, shift)['center_x'],
      center_y = circle_for(no, alpha, value, shift)['center_y'],
      radius = circle_for(no, alpha, value, shift)['radius']
    ) %>%
    mutate(type = type)
}

color_names <- crossing(
    tibble(type = c('full', 'value')),
    tibble(feature = colnames(select(features, -genre, -subgenre)))
  ) %>%
  mutate(color_name = interaction(type, feature))

fill_colors <- c(
  '#e8f0f5', '#e6f5f0', '#fdf7e5', '#fbe7e8', '#f0ebf4',
  '#2074a0', '#11a66e', '#efb605', '#e01a25', '#734098'
)

names(fill_colors) <- color_names$color_name

plot_for_genre <- function(genre, size, font_size) {
  triangles <- triangles_for(genre, 'value')
  full_triangles <- triangles_for(genre, 'full')
  circles <- circles_for(genre, 'value')
  full_circles <- circles_for(genre, 'full')

  ggplot() +
    geom_circle(
      data = full_circles,
      aes(
        x0 = center_x, y0 = center_y, r = radius,
        fill = interaction(type, feature),
        color = interaction(type, feature)
      ),
      size = size,
      show.legend = FALSE
    ) +
    geom_shape(
      data = full_triangles,
      aes(
        x = x, y = y, fill = interaction(type, feature),
        color = interaction(type, feature)
      ),
      size = size,
      show.legend = FALSE
    ) +
    geom_circle(
      data = circles,
      aes(
        x0 = center_x, y0 = center_y, r = radius,
        fill = interaction(type, feature),
        color = interaction(type, feature)
      ),
      size = size,
      show.legend = FALSE
    ) +
    geom_shape(
      data = triangles,
      aes(
        x = x, y = y,
        fill = interaction(type, feature),
        color = interaction(type, feature)
      ),
      size = size,
      show.legend = FALSE
    ) +
    geom_text(
      data = tibble(genre = !!genre, x = 0, y = -2.75),
      aes(x = x, y = y, label = genre),
      hjust = 0.5, family = 'Neucha', size = font_size
    ) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = fill_colors) +
    xlim(-2.4, 2.4) +
    ylim(-3, 2.4) +
    coord_fixed() +
    theme_void()
}

transform <- Vectorize(function(value) {
  1 / 2 / 1.5 * value + 1 / 2
})

genres <- features %>%
  distinct(genre) %>%
  arrange(genre) %>%
  mutate(
    no = row_number(),
    x = transform(cos((no - 1) / n() * 2 * pi)),
    y = transform(sin((no - 1) / n() * 2 * pi))
  )

genres_plot <- reduce(transpose(genres), .init = ggdraw(), function(p, row) {
  p + draw_plot(
    plot_for_genre(row[['genre']], 0.75, 4),
    x = row[['x']] - 0.075,
    y = row[['y']] - 0.075 - offset_y,
    width = 0.15, height = 0.15
  )
})

coord_shift <- Vectorize(function(subno) {
  shift_x <- 0.25
  shift_y <- 0.25
  if (subno > 2) shift_x <- -shift_x
  if (subno %% 2 == 0) shift_y <- -shift_y

  c(shift_x, shift_y)
})

subgenres <- features %>%
  distinct(genre, subgenre) %>%
  arrange(genre, subgenre) %>%
  inner_join(genres, by = 'genre') %>%
  group_by(genre) %>%
  mutate(subno = row_number()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    x = transform(cos((no - 1) / n() / 6 * 2 * pi) + coord_shift(subno)[1]),
    y = transform(sin((no - 1) / n() / 6 * 2 * pi) + coord_shift(subno)[2])
  )

subgenres_plot <- reduce(
  transpose(subgenres), .init = genres_plot, function(p, row) {
    p + draw_plot(
      plot_for_genre(row[['subgenre']], 0.35, 1.5),
      x = row[['x']] - 0.04,
      y = row[['y']] - 0.04 - offset_y,
      width = 0.08, height = 0.08
    )
  }
)

plot_legend <- function() {
  full_triangles <- triangles_for('Rap', 'full')
  full_circles <- circles_for('Rap', 'full')

  half_angle <- 1 / 2 / no_features * 360
  radius <- full_circles[1, ][['radius']]
  midpoint <- (1 + 1 / sin(half_angle / 180 * pi)) * radius / 2 + shift + 0.1

  full_circles <- full_circles %>%
    mutate(
      angle = (no - 1) / no_features * 360 + half_angle + 90,
      x = midpoint * cos(angle / 180 * pi),
      y = midpoint * sin(angle / 180 * pi),
      angle = (angle - 90) %% 180 + 270
    )

  ggplot() +
    geom_circle(
      data = full_circles,
      aes(
        x0 = center_x, y0 = center_y, r = radius,
        fill = interaction('value', feature),
        color = interaction('value', feature)
      ),
      size = 1,
      show.legend = FALSE
    ) +
    geom_shape(
      data = full_triangles,
      aes(
        x = x, y = y,
        fill = interaction('value', feature),
        color = interaction('value', feature)
      ),
      size = 1,
      show.legend = FALSE
    ) +
    geom_text(
      data = full_circles,
      aes(x = x, y = y, label = feature, angle = angle),
      hjust = 0.5, family = 'Neucha', size = 2.5, color = '#ffffff'
    ) +
    scale_fill_manual(values = fill_colors[6:10]) +
    scale_color_manual(values = fill_colors[6:10]) +
    xlim(-2.4, 2.4) +
    ylim(-2.4, 2.4) +
    coord_fixed() +
    theme_void()
}

size <- 0.2
subgenres_plot +
  draw_plot(
    plot_legend(),
    x = 0.5 - size / 2, y = 0.5 - size / 2 + 0.005 - offset_y,
    width = size, height = size
  ) +
  draw_label(
    'Spotify Audio Features per Genre & Subgenre',
    y = 0.95, fontfamily = 'Neucha', size = 17
  ) +
  draw_label(
    '#tidytuesday 04|2020',
    y = 0.9, fontfamily = 'Neucha', size = 10, fontface= 'bold'
  ) +
  draw_label(
    paste(format(nrow(tracks), big.mark = ','), 'Tracks'),
    x = 0.5, y = 0.35, fontfamily = 'Neucha', size = 7, fontface= 'bold'
  ) +
  draw_label(
    '@spren9er',
    x = 0.93, y = 0.045, fontfamily = 'Neucha', size = 4.5
  )

ggsave('images/tidytuesday_202004_spotify.png', dpi = 300)
