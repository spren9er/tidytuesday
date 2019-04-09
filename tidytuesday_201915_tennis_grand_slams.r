library(circlize)
library(magick)

path <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/'

players <- read_csv(paste0(path, 'player_dob.csv'))
grand_slams <- read_csv(paste0(path, 'grand_slam_timeline.csv'))

last_round <- function(outcome) {
  case_when(
    outcome == 'Won'             ~   1,
    outcome == 'Finalist'        ~   2,
    outcome == 'Semi-finalist'   ~   4,
    outcome == 'Quarterfinalist' ~   8,
    outcome == '4th Round'       ~  16,
    outcome == '3rd Round'       ~  32,
    outcome == '2nd Round'       ~  64,
    outcome == '1st Round'       ~ 128
  )
}

grand_slams <- grand_slams %>%
  mutate(
    last_round = last_round(outcome),
    gender = fct_recode(gender, 'female' = 'Female', 'male' = 'Male')
  ) %>%
  filter(!is.na(last_round))

best_players <- grand_slams %>%
  left_join(players, by = c('player' = 'name')) %>%
  filter(date_of_birth > '1981-01-01') %>%
  group_by(gender, player) %>%
  summarize(matches = n(), total = sum(last_round[last_round == 1])) %>%
  group_by(gender) %>%
  top_n(4, total) %>%
  arrange(gender, total)

best_players_grand_slams <- grand_slams %>%
  filter(player %in% pull(best_players, player), last_round <= 4) %>%
  select(player, gender, year, tournament, last_round)

player_tournament <- best_players_grand_slams %>%
  filter(last_round == 1) %>%
  group_by(player, tournament) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  transmute(player, to = tournament, total)

player_year <- best_players_grand_slams %>%
  filter(last_round == 1) %>%
  group_by(player, year) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  transmute(player, to = as.character(year), total)

player_last_round <- best_players_grand_slams %>%
  group_by(player, last_round) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  transmute(player, to = as.character(last_round), total)

imap(best_players$player, function(player, idx) {
  adjacency_list <-
    bind_rows(player_tournament, player_year, player_last_round) %>%
    mutate(
      color = case_when(
        player == !!player & str_starts(to, 'A') ~ '#c54950',
        player == !!player & str_starts(to, 'F') ~ '#2a9e46',
        player == !!player & str_starts(to, 'W') ~ '#3766aa',
        player == !!player & str_starts(to, 'U') ~ '#6b42b8',
        player == !!player & str_detect(to, '\\d{4}') ~ '#777777',
        player == !!player & str_starts(to, '1') ~ '#c54950',
        player == !!player & str_starts(to, '2') ~ '#2a9e46',
        player == !!player & str_starts(to, '4') ~ '#3766aa',
        TRUE ~ '#efefef80'
      ),
      to = case_when(
        to == 1 ~ 'Champion',
        to == 2 ~ 'Final',
        to == 4 ~ 'Semi-Final',
        TRUE ~ to
      ),
      rank = if_else(color == '#efefef80', 1, 2)
    )

  years <- sort(unique(player_year$to))
  year_colors <-  rep('#dedede', length(years))
  names(year_colors) <- years

  players <- best_players$player
  player_colors <- rep('#dedede', length(players))
  names(player_colors) <- players

  colors <- c(
    'Australian Open' = '#c54950', 'French Open' = '#2a9e46',
    'Wimbledon' = '#3766aa', 'US Open' = '#6b42b8',
    year_colors,
    'Champion' = '#c54950', 'Final' = '#2a9e46', 'Semi-Final' = '#3766aa',
    player_colors
  )

  player_years <- adjacency_list %>%
    filter(player == !!player, str_detect(to, '\\d{4}')) %>%
    pull(as.integer(to))

  colors[player_years] <- '#777777'
  colors[player] <- '#333333'

  png(
    file = paste0('chord_diagram_', idx, '.png'),
    height = 7, width = 7,  units = 'in', res = 300
  )

  circos.clear()
  circos.par(
    gap.after = c(
      rep(2, 3), 9, rep(2, length(years) - 1), 9, rep(2, 2), 15,
      rep(2, 7), 15
    ),
    start.degree = 90
  )

  par(
    col = '#333333', col.main = '#333333', mar = c(0, 0, 3.1, 0), bg = '#fef9f4'
  )

  styles <- list(
    track.index = 1, panel.fun = function(x, y) {
      circos.text(
        CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
        facing = 'clockwise', niceFacing = TRUE, adj = c(-0.025, 0.5),
        cex = 0.6)
    },
      bg.border = NA
    )

  chordDiagram(
    adjacency_list %>%
      select(player, to, total),
    order = names(colors),
    grid.col = colors,
    col = pull(adjacency_list, color),
    transparency = 0.4,
    annotationTrack = 'grid',
    preAllocateTracks = list(list(track.height = 0.2)),
    link.rank = pull(adjacency_list, rank),
  );do.call(circos.track, styles)

  text(-1, -1, '#tidytuesday 15|2019', cex = 0.5)
  text(1, -1, '© 2019 spren9er', cex = 0.5)
  title('Grand Slam Heroes of the Modern Era')

  dev.off()
})

frames <- map(1:8, function(idx) {
  file = paste0('chord_diagram_', idx, '.png')
  img <- image_read(file)
  image_scale(img, '1024x1024')
})

animation <- image_animate(image_join(frames), fps = 0.5)

image_write(
  image = animation,
  path = 'tidytuesday_201915_tennis_grand_slams.gif',
  quality = 100
)
