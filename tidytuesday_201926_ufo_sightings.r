library(tidyverse)
library(janitor)
library(lubridate)
library(tweenr)
library(rayshader)
library(magick)
library(maps)
library(av)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-06-25/'
  )

ufo_sightings <- read_csv(
  paste0(path, 'ufo_sightings.csv'),
  col_types = list(date_time = col_datetime('%m/%d/%Y %H:%M'))
)

ufo_sightings.us <- ufo_sightings %>%
  mutate(year = year(date_time)) %>%
  filter(country == 'us', !state %in% c('ak', 'hi', 'pr'))

min_long.us <- -124.848974
max_long.us <- -66.885444
min_lat.us <- 24.396308
max_lat.us <- 49.384358

long_width <- max_long.us - min_long.us
lat_height <- max_lat.us - min_lat.us

factor <- 2
size_x <- ceiling(factor * long_width)
size_y <- ceiling(factor * lat_height)

ufo_sightings_us_for_year <- function(year) {
  ufo_coordinates <- ufo_sightings.us %>%
    filter(year == !!year)

  kde <- MASS::kde2d(
    pull(ufo_coordinates, longitude),
    pull(ufo_coordinates, latitude),
    n = c(size_x, size_y),
    h = 1,
    lims = c(min_long.us, max_long.us, min_lat.us, max_lat.us)
  )

  bind_cols(
    crossing(x = kde$x, y = kde$y),
    height = as.vector(t(kde$z))
  )
}

start_year <- 1990
end_year <- 2014

ufo_sightings.us.total <- reduce(
  start_year:end_year,
  .init = tibble(),
  function(agg, year) {
    bind_rows(
      agg,
      ufo_sightings_us_for_year(year) %>%
        mutate(year = !!year)
    )
  }
)

(ufo_sightings.us.agg <- ufo_sightings.us.total %>%
  group_by(year) %>%
  summarize(min_height = min(height), max_height = max(height)))

max_height <- ufo_sightings.us.agg %>%
  summarize(max_height = max(max_height)) %>%
  pull(max_height)

ufo_sightings.us.total <- ufo_sightings.us.total %>%
  mutate(height = height / max_height)

ufo <- split(ufo_sightings.us.total, ufo_sightings.us.total$year)

ufo_frames <- reduce(
  (start_year + 1):end_year,
  .init = ufo[[!!start_year]],
  function(agg, year) {
    tween_state(agg, ufo[[as.character(year)]], ease = 'linear', nframes = 20)
  }
)

ufo_frames <- split(ufo_frames, ufo_frames$.frame)

theme_bare <- theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 8),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank()
)

us.df <- map_data('state')

p_base <- function(year) {
  ggplot() +
    geom_polygon(
      data = us.df,
      aes(x = long, y = lat, group = group),
      fill = '#efefef', size = 0, show.legend = FALSE
    ) +
    geom_path(
      data = us.df,
      aes(x = long, y = lat, group = group),
      size = 0.25, color = '#333333'
    ) +
    geom_tile(
      aes(x = x, y = y, fill = ifelse(height < 0.01, NA, height)),
      alpha = 1, size = 0, show.legend = FALSE, color = 'transparent'
    ) +
    scale_fill_viridis_c(option = 'C', na.value = NA) +
    coord_quickmap() +
    labs(
      title = 'Ufo Sightings in United States',
      subtitle = paste('tidytuesday 26|2019', year, sep = '  •  '),
      caption = '© 2019 spren9er'
    ) +
    theme_bare +
    theme(
      text = element_text(family = 'Bitter'),
      panel.background = element_rect(fill = '#333333', color = NA),
      plot.background = element_rect(fill = '#333333', color = NA),
      plot.title = element_text(
        margin = margin(b = 8), size = 14, hjust = 0.5, face = 'bold',
        color = '#efefef'
      ),
      plot.subtitle = element_text(
        size = 11, hjust = 0.5, face = 'plain', color = '#efefef'
      ),
      plot.caption = element_text(
        color = '#cccccc', size = 6, margin = margin(t = -11), hjust = 0.88
      ),
      plot.margin = margin(t = 20, b = 66.15)
    )
}

filename <- function(id) {
  paste0('images/tidytuesday_201926_ufo_sightings/ufo_', id, '.png')
}

for (d in ufo_frames) {
  year <- floor(d$year[[1]])
  p <- p_base(year) %+% d
  id <- str_pad(d$.frame[1], width = 5, side = 'left', pad = '0')

  plot_gg(
    plot(p),
    shadow_intensity = 0.8, scale = 300, shadow = FALSE, width = 8, height = 6,
    solid = FALSE, background = '#efefef', baseshape = 'rectangle',
    windowsize = c(1200, 800), offset_edges = TRUE
  )
  render_camera(phi = 55, theta = 0, zoom = 0.41, fov = 60)
  render_snapshot(filename(id), clear = TRUE)
}

frames <- purrr::map(1:length(ufo_frames), function(idx) {
  id <- str_pad(idx, width = 5, side = 'left', pad = '0')
  file <- filename(id)
  img <- image_read(file)
  img <- image_scale(img, '900x600')
  if (idx %% 20 != 0) return(img)
  return(rep(img, 20))
})

animation <- image_animate(image_join(frames), fps = 20)

image_write(
  image = animation,
  path = 'images/tidytuesday_201926_ufo_sightings.gif',
  quality = 100
)

frames.files <- unlist(purrr::map(0:480, function(idx) {
  id <- str_pad(idx, width = 5, side = 'left', pad = '0')
  file <- filename(id)
  if (idx %% 20 != 0) return(file)
  rep(file, 20)
}))

av_encode_video(
  frames.files,
  'images/tidytuesday_201926_ufo_sightings.mp4',
  framerate = 24
)
