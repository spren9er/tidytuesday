library(tidyverse)
library(av)
library(gganimate)
library(ggbeeswarm)
library(lubridate)

raw_data <- read_csv(
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-04-23/tidy_anime.csv'
  )
)

anime <- raw_data %>%
  select(anime_id = animeID, everything(), -synopsis, -background) %>%
  filter(!type %in% c('Music', 'Unknown'), rating != 'None') %>%
  distinct(anime_id, .keep_all = TRUE) %>%
  mutate(
    year = year(start_date),
    rating = fct_relevel(
      fct_recode(
        rating,
        'G'     = 'G - All Ages',
        'PG'    = 'PG - Children',
        'PG-13' = 'PG-13 - Teens 13 or older',
        'R'     = 'R+ - Mild Nudity',
        'NC-17' = 'R - 17+ (violence & profanity)'
      ),
      c('G', 'PG', 'PG-13', 'R', 'NC-17')
    )
  )

start_year <- 1978
end_year <- 2018
anime_years <- reduce(
  start_year:end_year, .init = tibble(),
  function(agg, year) {
    bind_rows(
      agg,
      anime %>%
        filter(year >= start_year, year <= !!year) %>%
        mutate(until_year = !!year)
    )
  }
)

animation <- anime_years %>%
  ggplot(aes(x = rating, y = score)) +
    geom_quasirandom(alpha = 0.15) +
    geom_violin(fill = 'transparent', draw_quantiles = c(0.5)) +
    scale_y_continuous(
      breaks = 0:10, labels = 0:10, minor_breaks = NULL, expand = c(0, 0)
    ) +
    expand_limits(y = 0) +
    labs(
      x = 'Rating',
      y = 'Score',
      title = 'Score Distributions of Animes per Rating',
      subtitle = paste(
        '#tidytuesday 17|2019',
        '{start_year} - {floor(frame_time)}',
        sep = ' • '
      ),
      caption = '© 2019 spren9er'
    ) +
    transition_time(until_year) +
    enter_fade() +
    ease_aes('linear')

animate(
  animation,
  nframes = end_year - start_year + 1, duration = 8,
  renderer = av_renderer(), width = 1000, height = 1000
)

anim_save('images/tidytuesday_201917_anime.mp4')
