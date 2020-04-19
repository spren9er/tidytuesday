library(tidyverse)
library(ggforce)
library(viridis)

data <- tidytuesdayR::tt_load(2020, week = 16)

# %%
top_n_artists <- 23
max_rank_title <- 100
offset_x <- 0.2
offset_y <- 0.025
pad <- 3
parallel_extend <- 1

# %%
rankings <- data$rankings %>%
  rename(rank_title = ID) %>%
  mutate(
    artist =
      case_when(
        artist == 'UGK ft. OutKast' ~ 'OutKast',
        artist == 'Wu-Tan Clan' ~ 'Wu-Tang Clan',
        str_starts(artist, 'The Notorious B.I.G') ~ 'The Notorious B.I.G.',
        str_starts(artist, 'Kanye West') ~ 'Kanye West',
        str_starts(artist, 'Eminem') ~ 'Eminem',
        str_starts(artist, '2Pac') ~ '2Pac',
        str_starts(artist, 'Nas') ~ 'Nas',
        str_starts(artist, 'Missy Elliott') ~ 'Missy Elliott',
        str_starts(artist, 'Kendrick Lamar') ~ 'Kendrick Lamar',
        str_starts(artist, 'Drake') ~ 'Drake',
        str_starts(artist, 'Dr Dre') ~ 'Dr. Dre',
        str_starts(artist, 'Gang Starr') ~ 'Gang Starr',
        str_starts(artist, 'Mobb Deep') ~ 'Mobb Deep',
        TRUE ~ artist
      ),
    artist = str_replace(artist, 'OutKast', 'Outkast'),
    artist = str_replace(artist, 'JAY-Z', 'Jay-Z'),
    title = ifelse(title == 'm.A.A.d. city', 'M.A.A.D. City', title),
    title = str_replace(title, '’', '\''),
    title = str_replace(title, '‘', '\''),
    title = str_replace(title, '’', '\'')
  ) %>%
  select(rank_title:gender) %>%
  group_by(artist) %>%
  mutate(total_artist = n()) %>%
  ungroup()

# %%
label_max_rank_title <- paste0(max_rank_title + 1, '. - 311.')

label_title <- function(rank_title, title) {
  return(paste0(str_pad(rank_title, 3, pad = 0), '. ', title))
}

length_title <- function(rank_title) {
  if (rank_title == max_rank_title + 1) {
    len <- str_length(label_max_rank_title)
  } else {
    row <- filter(rankings, rank_title == !!rank_title)
    title <- label_title(rank_title, row$title)
    len <- str_length(title)
  }

  return((len / 2)^(0.95) * 0.00825)
}

title_labels <- rankings %>%
  filter(rank_title <= max_rank_title + 1) %>%
  mutate(
    label = ifelse(
      rank_title == max_rank_title + 1,
      label_max_rank_title,
      label_title(rank_title, title)
    )
  )

# %%
top_artists <- rankings %>%
  select(artist, total_artist) %>%
  distinct(artist, .keep_all = TRUE) %>%
  arrange(desc(total_artist), artist) %>%
  mutate(rank_artist = row_number()) %>%
  head(top_n_artists) %>%
  arrange(total_artist, desc(artist)) %>%
  mutate(cum_total_artist = cumsum(total_artist))

artist_title <- top_artists %>%
  select(-total_artist) %>%
  left_join(rankings, by = 'artist') %>%
  mutate(
    rank_title = ifelse(
      rank_title > max_rank_title,
      max_rank_title + 1,
      rank_title
    )
  ) %>%
  group_by(rank_artist, artist, rank_title) %>%
  summarize(artist_size = n()) %>%
  ungroup() %>%
  arrange(desc(rank_artist), desc(rank_title)) %>%
  mutate(
    group = row_number(),
    shift = if_else(
      rank_artist - lag(rank_artist, default = top_n_artists) < 0,
      pad,
      0
    ),
    cum_shift = cumsum(shift),
    artist_end = cumsum(artist_size) + cum_shift,
    artist_start = lag(artist_end, default = 0) + shift
  ) %>%
  group_by(rank_artist, artist) %>%
  mutate(
    total_artist_size = sum(artist_size),
    midpoint = min(artist_start) + total_artist_size / 2
  )

artist_ticks <- max(artist_title$artist_end)

artist_title_parallel <- artist_title %>%
  rowwise() %>%
  mutate(
    x1 = length_title(rank_title),
    x2 = length_title(rank_title),
    x3 = parallel_extend,
    x4 = parallel_extend,
    y1 = -2 / (max_rank_title + 1) * (rank_title - 0.075) + 1,
    y2 = -2 / (max_rank_title + 1) * (rank_title + 0.075) + 1,
    y3 = 2 / artist_ticks * artist_start - 1,
    y4 = 2 / artist_ticks * artist_end - 1
  ) %>%
  pivot_longer(
    c(x1, x2, x3, x4, y1, y2, y3, y4),
    names_to = c('.value', 'set'),
    names_pattern = '(.)(.)',
    values_to = c('x', 'y')
  )

# %%
year_title <- rankings %>%
  mutate(
    rank_title = ifelse(
      rank_title > max_rank_title,
      max_rank_title + 1,
      rank_title
    )
  ) %>%
  group_by(year, rank_title) %>%
  summarize(year_size = n()) %>%
  ungroup() %>%
  arrange(year, desc(rank_title)) %>%
  mutate(
    group = row_number(),
    shift = if_else(year - lag(year, default = min(rankings$year)) > 0, pad, 0),
    cum_shift = cumsum(shift),
    year_end = cumsum(year_size) + cum_shift,
    year_start = lag(year_end, default = 0) + shift
  ) %>%
  group_by(year) %>%
  mutate(
    total_year_size = sum(year_size),
    midpoint = min(year_start) + total_year_size / 2
  )

year_ticks <- max(year_title$year_end)

year_title_parallel <- year_title %>%
  rowwise() %>%
  mutate(
    x1 = -length_title(rank_title),
    x2 = -length_title(rank_title),
    x3 = -parallel_extend,
    x4 = -parallel_extend,
    y1 = -2 / (max_rank_title + 1) * (rank_title - 0.075) + 1,
    y2 = -2 / (max_rank_title + 1) * (rank_title + 0.075) + 1,
    y3 = 2 / year_ticks * year_start - 1,
    y4 = 2 / year_ticks * year_end - 1
  ) %>%
  pivot_longer(
    c(x1, x2, x3, x4, y1, y2, y3, y4),
    names_to = c('.value', 'set'),
    names_pattern = '(.)(.)',
    values_to = c('x', 'y')
  )

# %%
total_year <- n_distinct(rankings$year)

fill_colors <- c(
  viridis_pal(option = 'A')(total_year),
  viridis_pal()(top_n_artists)
)

names(fill_colors) <- c(
  as.character(-sort(unique(rankings$year))),
  as.character(-unique(artist_title$rank_artist))
)

# %%
ggplot() +
  geom_diagonal_wide(
    data = arrange(year_title_parallel, group),
    aes(
      x = x,
      y = y,
      group = group,
      fill = as.factor(-year),
      alpha = ifelse(rank_title > max_rank_title, 'dim_year', 'full_year')
    ),
    strength = 0.5, n = 5000, show.legend = FALSE
  ) +
  geom_diagonal_wide(
    data = arrange(artist_title_parallel, group),
    aes(
      x = x,
      y = y,
      group = group,
      fill = as.factor(-rank_artist),
      alpha = ifelse(rank_title > max_rank_title, 'dim_artist', 'full_artist')
    ),
    strength = 0.5, n = 5000, show.legend = FALSE
  ) +
  geom_text(
    data = title_labels,
    aes(
      x = 0,
      y = -2 / (max_rank_title + 1) * rank_title + 1,
      label = label
    ),
    hjust = 0.5, vjust = 0.5, size = 0.9, lineheight = 0.9,
    family = 'Roboto Slab', color = '#333333'
  ) +
  geom_text(
    data = distinct(year_title, year, .keep_all = TRUE),
    aes(
      x = -parallel_extend,
      y = 2 / year_ticks * midpoint - 1,
      label = year
    ),
    hjust = 1, vjust = 0.5, size = 0.9, lineheight = 0.9, nudge_x = -0.01,
    family = 'Roboto Slab', color = '#333333'
  ) +
  geom_text(
    data = distinct(artist_title, artist, .keep_all = TRUE),
    aes(
      x = parallel_extend,
      y = 2 / artist_ticks * midpoint - 1,
      label = paste0(artist, ' (', total_artist_size, ')')
    ),
    hjust = 0, vjust = 0.5, size = 0.9, lineheight = 0.9, nudge_x = 0.01,
    family = 'Roboto Slab', color = '#333333'
  ) +
  scale_alpha_manual(
    values = c(
      'dim_year' = 0.2, 'full_year' = 0.8,
      'dim_artist' = 0.2, 'full_artist' = 0.8
    )
  ) +
  scale_fill_manual(values = fill_colors) +
  xlim(-1.1, 1.1) +
  coord_fixed() +
  labs(
    title = paste('The', max_rank_title, 'Greatest Hip-Hop Songs Of All Time'),
    subtitle = 'BBC Music  •  #tidytuesday 16|2020',
    caption = '© 2020 spren9er'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5, margin = margin(t = 10)),
    plot.subtitle = element_text(
      size = 6, hjust = 0.5, face = 'bold', margin = margin(t = 7, b = 0)
    ),
    plot.caption = element_text(
      size = 2.5, color = '#dedede', hjust = 0.5, margin = margin(t = 0)
    ),
    text = element_text(family = 'Roboto Slab', color = '#333333')
  )

# %%
ggsave(
  'images/tidytuesday_202016_best_rap_artists.png',
  dpi = 600
)
