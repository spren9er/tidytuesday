library(tidyverse)
library(gganimate)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-11-19/'
  )

data <- read_csv(paste0(path, 'nz_bird.csv'))

votes <- data %>%
  transmute(
    id = (row_number() - 1) %/% 5 + 1,
    rank = as.integer(str_extract(vote_rank, '\\d')),
    bird = bird_breed
  ) %>%
  mutate(bird = str_to_title(bird)) %>%
  drop_na

top_ranked_birds <- function(votes) {
  votes %>%
    arrange(id, rank) %>%
    group_by(id) %>%
    top_n(1, -rank) %>%
    ungroup
}

aggregate <- function(votes) {
  votes %>%
    count(bird, name = 'total', sort = TRUE) %>%
    mutate(rank = row_number()) %>%
    select(rank, everything())
}

next_ballot <- function(votes, birds_ranking, bird_to_remove) {
  new_votes <- filter(votes, bird != bird_to_remove)

  new_birds_ranking <- aggregate(top_ranked_birds(new_votes))
  removed_birds <- filter(birds_ranking, total == 0)
  new_birds_ranking <- bind_rows(
    new_birds_ranking,
    tibble(
      rank = nrow(new_birds_ranking) + 1,
      bird = bird_to_remove,
      total = 0
    ),
    removed_birds
  )

  removed_bird_distribution <- birds_ranking %>%
    select(-rank) %>%
    inner_join(rename(new_birds_ranking, new_total = total), by = 'bird') %>%
    transmute(rank, bird, total = new_total - total)

  max_percentage <- new_birds_ranking %>%
    mutate(percentage = total / sum(total)) %>%
    summarize(max_percentage = max(percentage)) %>%
    pull(max_percentage)

  tibble(
    votes = list(new_votes),
    birds_ranking = list(new_birds_ranking),
    removed_bird_distribution = list(removed_bird_distribution),
    max_percentage = max_percentage
  )
}

compute_ballots <- function(votes, birds_ranking, round) {
  bird_to_remove <- birds_ranking %>%
    filter(total != 0) %>%
    tail(1) %>%
    pull(bird)

  new_ballot = next_ballot(votes, birds_ranking, bird_to_remove)
  ballots <<- bind_rows(ballots, mutate(new_ballot, round = round))

  if (pull(new_ballot, max_percentage) > 0.5) return()

  compute_ballots(
    new_ballot$votes[[1]],
    new_ballot$birds_ranking[[1]],
    round + 1
  )
}

ballots <- tibble()
birds_ranking <- aggregate(top_ranked_birds(votes))
compute_ballots(votes, birds_ranking, 1)
(top_birds <- head(tail(ballots$birds_ranking, 1)[[1]], 10))

n_ballots <- 84
range <- 75:n_ballots
sliced_ballots <- filter(ballots, round %in% range)

create_frames_for <- function(column, indexes, frames, limit = 10) {
  indexes <- reduce(1:length(range), .init = c(), ~ c(.x, indexes + .y))
  indexes <- indexes[indexes < length(range)]

  frames <- sort(
    reduce(frames, .init = c(), ~ c(.x, seq(.y, n_ballots, by = 5)))
  )[1:length(indexes)]

  bind_rows(
    map2(
      sliced_ballots[[column]][indexes],
      frames,
      ~ mutate(head(.x, limit), frame = .y)
    )
  )
}

# animation story board
#
#              | 1 | 2 | 3 | 4 | 5 || 6 | 7 | 8 | 9 | 10 | ...
# -----------------------------------------------------------=
# main_frame   | 1 | - | - | 1 | - || 2 | - | - | 2 |  - | ...
# black_bar    | 0 | - | 1 | - | - || 1 | - | 2 | - |  - | ...
# distribution | h | u | 1 | 1 | - || h | u | 2 | 2 |  - | ...

top_birds_ranking_frames <-
  create_frames_for('birds_ranking', c(0, 0), c(1, 4)) %>%
  filter(frame <= 41)

bird_to_remove_frames <-
  create_frames_for('birds_ranking', 0, 1) %>%
  filter(total > 0) %>%
  group_by(frame) %>%
  top_n(1, -total) %>%
  ungroup %>%
  filter(rank > 2)

bird_to_remove_frames <- map_df(
  1:8 * 5 - 1.8,
  ~ bird_to_remove_frames %>%
    filter(frame <= .x) %>%
    mutate(frame = .x)
  )

bird_to_remove_frames <- bind_rows(
  bird_to_remove_frames,
  mutate(filter(bird_to_remove_frames, frame == 38.2), frame = 37),
  tibble(rank = 11, bird = '-', total = 0, frame = -3)
  ) %>%
  mutate(frame = frame + 4) %>%
  filter(frame < 42)

removed_bird_distribution_frames <-
  create_frames_for(
    'removed_bird_distribution', c(1, 1), c(3, 4), limit = nrow(top_birds)
  ) %>%
  mutate(
    bird = factor(bird, levels = top_birds$bird),
    total = pmax(total, 0)
  ) %>%
  arrange(frame, bird) %>%
  group_by(frame) %>%
  mutate(cum_total = cumsum(lag(total, default = 0))) %>%
  ungroup()

removed_bird_distribution_frames <- bind_rows(
  removed_bird_distribution_frames,
  mutate(removed_bird_distribution_frames, frame = frame - 0.85),
  removed_bird_distribution_frames %>%
    filter((frame - 3) %% 5 == 0) %>%
    mutate(
      total = 0,
      cum_total = 0,
      frame = frame - 1
    ),
  removed_bird_distribution_frames %>%
    filter((frame - 3) %% 5 == 0) %>%
    mutate(
      total = 0,
      cum_total = 0,
      frame = frame - 2
    ),
  removed_bird_distribution_frames %>%
    filter(frame == 3) %>%
    mutate(
      total = 0,
      cum_total = 0,
      frame = 41
    )
  )

min_x <- -5000
max_x <- 12000

fill_colors <- c(
  '#efb605',
  '#fe7e2e',
  '#e53013',
  '#db1d74',
  '#93147b',
  '#66489f',
  '#218ccc',
  '#1b408f',
  '#7fb955',
  '#12a66e',
  '#333333'
)

names(fill_colors) <- c(
  'Yellow-Eyed Penguin',
  'Kākāpō',
  'Banded Dotterel',
  'Black Robin',
  'Fantail',
  'New Zealand Falcon',
  'Kererū',
  'Blue Duck',
  'Kea',
  'Kākā'
)

animation <- top_birds_ranking_frames %>%
  ggplot(aes(x = total / 2, y = -rank)) +
    geom_tile(
      aes(width = total, fill = bird, group = bird),
      height = 0.7, show.legend = FALSE
    ) +
    geom_tile(
      data = bird_to_remove_frames,
      aes(width = total,  group = bird),
      fill = '#333333', height = 0.7, show.legend = FALSE
    ) +
    geom_tile(
      data = removed_bird_distribution_frames,
      aes(
        x = cum_total + total / 2,
        y = -10 + (frame - 2) %/% 5 ,
        width = total,
        fill = bird, group = bird
      ),
      height = 0.7, show.legend = FALSE
    ) +
    geom_text(
      aes(x = min_x, y = -rank, label = paste0(rank, '. ', bird), group = bird),
      hjust = 0, size = 7, family = 'Bree Serif'
    ) +
    scale_fill_manual(values = fill_colors) +
    scale_y_continuous(breaks = NULL, limits = c(-10.4, -0.6)) +
    scale_x_continuous(breaks = c(0, 5000, 10000), limits = c(min_x, max_x)) +
    theme(
      text = element_text(family = 'Bree Serif'),
      panel.border = element_blank(),
      panel.background = element_rect(fill = '#f9f9fa', color = 'transparent'),
      panel.grid.major = element_line(size = 0.3, color = '#cccccc'),
      plot.background = element_rect(fill = '#f9f9fa', color = 'transparent'),
      plot.title = element_text(
        face = 'bold', hjust = 0.5, size = 31, margin = margin(t = 45, b = 10)
      ),
      plot.subtitle = element_text(
        face = 'plain', hjust = 0.5, size = 24, margin = margin(t = 10, b = 25)
      ),
      plot.caption = element_text(
        face = 'plain', color = '#cccccc', size = 16, hjust = 0.5,
        margin = margin(t = 30, b = 10)
      ),
      plot.margin = margin(b = 10, r = 10, l = 10),
      axis.text.x = element_text(face = 'plain', size = 16, color = '#cccccc'),
      axis.title = element_text(
        face = 'bold', size = 20, color = '#333333',
        margin = margin(t = 25, b = 10)
      )
    ) +
    labs(
      x = 'Total Votes',
      y = '',
      title = 'Bird of the Year 2019  •  New Zealand',
      subtitle = 'Last Rounds of Instant-Runoff Voting (IRV)',
      caption = '#tidytuesday 47|2019  •  spren9er'
    ) +
    transition_time(frame) +
    ease_aes('sine-in-out') +
    enter_drift(x_mod = -1) +
    exit_drift(x_mod = 1)

animate(
  animation,
  duration = 30, fps = 40, start_pause = 60, end_pause = 100,
  width = 1000, height = 1000
)

anim_save('images/tidytuesday_201947_nz_bird_of_the_year.gif')
