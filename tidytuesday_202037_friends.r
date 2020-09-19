# %%
library(tidyverse)
library(ggforce)
library(grid)
library(cowplot)

data <- tidytuesdayR::tt_load(2020, week = 37)

texts <- data$friends

# %% [markdown]
# Clean up titles

# %%
episodes <- data$friends_info %>%
  select(season, episode, title) %>%
  mutate(
    title = str_to_title(title),
    title = ifelse(
      str_starts(title, "The One") & str_length(title) > 30,
      str_replace(
        str_replace(title, "One With", "One With\n"),
        "One Where",
        "One Where\n"
      ),
      title
    ),
    title = paste0(
      str_pad(episode, 2, pad = "0"), ". ", title
    ),
    title = str_replace(title, "C.h.e.e.s.e", "C.H.E.E.S.E")
  )

main_speakers <- c(
  "Ross Geller",
  "Joey Tribbiani",
  "Monica Geller",
  "Rachel Green",
  "Phoebe Buffay",
  "Chandler Bing"
)

# %% [markdown]
# Determine spoken text totals for each character

# %%
basic_text_totals <- texts %>%
  filter(speaker %in% c(main_speakers, "#ALL#")) %>%
  mutate(text_length = str_length(text)) %>%
  count(season, episode, scene, speaker, wt = text_length, name = "total") %>%
  arrange(season, episode, scene, speaker)

groups_per_scene <- basic_text_totals %>%
  group_by(season, episode, scene) %>%
  summarize(
    speakers = list(speaker),
    totals_per_scene = sum(total)
  )

all_text_totals <- basic_text_totals %>%
  filter(speaker == "#ALL#") %>%
  left_join(groups_per_scene, by = c("season", "episode", "scene")) %>%
  unnest(speakers) %>%
  filter(speakers != "#ALL#") %>%
  select(-speaker, -totals_per_scene) %>%
  rename(speaker = speakers)

text_totals <- bind_rows(basic_text_totals, all_text_totals) %>%
  count(season, episode, scene, speaker, wt = total, name = "total") %>%
  filter(speaker != "#ALL#")

# %% [markdown]
# Determine all spoken text totals from one specific speaker to all others

# %%
spoken_from <- function(df, speaker) {
  speakers <- text_totals %>%
    filter(speaker == !!speaker) %>%
    select(-speaker, -total) %>%
    left_join(text_totals, by = c("season", "episode", "scene"))

  spoken_from <- speakers %>%
    filter(speaker == !!speaker) %>%
    select(-speaker) %>%
    left_join(
      speakers %>%
      filter(speaker != !!speaker) %>%
      select(-total),
      by = c("season", "episode", "scene")
    ) %>%
    count(season, episode, speaker, wt = total, name = "total") %>%
    drop_na(speaker) %>%
    rename(target = speaker) %>%
    mutate(source = !!speaker) %>%
    select(season, episode, source, target, total)

  bind_rows(df, spoken_from)
}

direction <- function(x0, y0, x1, y1) {
  if (x1 > x0) return(1)
  if (x1 == x0 & y1 > y0) return(1)

  return(-1)
}

# %% [markdown]
# Convert exact points of hexagon to translated ones (for link connections)

# %%
transform_coords <- function(x0, y0, x1, y1, radius0, radius1, dist, sign) {
    if (direction(x0, y0, x1, y1) < 0) return(
      transform_coords(x1, y1, x0, y0, radius1, radius0, dist, -sign)
    )

    m <- (y1 - y0) / (x1 - x0)
    alpha <- atan(m)

    new_x0 <- x0 + radius0 * cos(alpha) - sign * dist * sin(alpha)
    new_y0 <- y0 + radius0 * sin(alpha) + sign * dist * cos(alpha)
    new_x1 <- x1 - radius1 * cos(alpha) - sign * dist * sin(alpha)
    new_y1 <- y1 - radius1 * sin(alpha) + sign * dist * cos(alpha)

    points <- c(new_x0, new_y0, new_x1, new_y1)
    names(points) <- c("x0", "y0", "x1", "y1")

    return(points)
}

angles <- seq(0, 5) * pi / 3

nodes <- tibble(
  speaker = main_speakers,
  x = map_dbl(angles, cos),
  y = map_dbl(angles, sin)
)

# %% [markdown]
# Compute all edges

# %%
edges <- reduce(main_speakers, spoken_from, .init = tibble()) %>%
  arrange(season, episode, source, target, total) %>%
  left_join(nodes, by = c("source" = "speaker")) %>%
  rename(source_x = x, source_y = y) %>%
  left_join(nodes, by = c("target" = "speaker")) %>%
  rename(target_x = x, target_y = y, link_total = total) %>%
  left_join(
    text_totals %>%
      count(season, episode, speaker, wt=total, name="total") %>%
      rename(source = speaker),
    by = c("season", "episode", "source")
  ) %>%
  rename(source_total = total) %>%
  left_join(
    text_totals %>%
      count(season, episode, speaker, wt=total, name="total") %>%
      rename(target = speaker),
    by = c("season", "episode", "target")
  ) %>%
  rename(target_total = total) %>%
  mutate(
    source_size = source_total / max(source_total),
    target_size = target_total / max(target_total)
  ) %>%
  rowwise() %>%
  mutate(
    source_radius = sqrt(source_size) / 2,
    target_radius = sqrt(target_size) / 2,
    padding = 0.06,
    points = list(transform_coords(
      source_x, source_y, target_x, target_y,
      source_radius + padding, target_radius + padding, 0.05, 1
    )),
    dir = direction(source_x, source_y, target_x, target_y),
    x0 = points[[1]],
    y0 = points[[2]],
    x1 = points[[3]],
    y1 = points[[4]]
  ) %>%
  select(season, episode, source, target, link_total, x0, y0, x1, y1, dir)

colors <- c("#c1d5d3", "#3a3845", "#a2a7aa", "#b3465a", "#e2dfcd", "#674f5e")
names(colors) <- main_speakers

# %% [markdown]
# Define function for plotting all network graphs per season

# %%
plot_season <- function(season) {
  # top ten edges for each episode of given season
  top_edges <- edges %>%
    group_by(season, episode) %>%
    top_n(10, wt = link_total) %>%
    ungroup() %>%
    arrange(season, episode, desc(link_total)) %>%
    filter(season == !!season) %>%
    left_join(episodes, by = c("season", "episode"))

  # bubbles
  circles <- text_totals %>%
    count(season, episode, speaker, wt = total, name = "total") %>%
    inner_join(nodes, by = "speaker") %>%
    mutate(
      size = total / max(total),
      radius = sqrt(size) / 2
    ) %>%
    filter(season == !!season) %>%
    left_join(episodes, by = c("season", "episode"))

  p <- circles %>%
    ggplot() +
    geom_circle(
      aes(x0 = x, y0 = y, r = radius, fill = speaker, color = speaker),
      show.legend = FALSE
    ) +
    geom_text(
      data = filter(circles, !speaker %in% c("Phoebe Buffay", "Ross Geller")),
      aes(x = x, y = y, label = map(str_split(speaker, " "), ~ .[1])),
      color = "#efefef", fontface = "bold", size = 2.5, family = "Open Sans"
    ) +
    geom_text(
      data = filter(circles, speaker %in% c("Phoebe Buffay", "Ross Geller")),
      aes(x = x, y = y, label = map(str_split(speaker, " "), ~ .[1])),
      color = "#555555", fontface = "bold", size = 2.5, family = "Open Sans"
    ) +
    geom_segment(
      data = filter(top_edges, dir > 0),
      aes(x = x0, xend = x1, y = y0, yend = y1, color = source),
      size = 0.75,
      arrow = arrow(
        length = unit(0.075, "inches"),
        type = "closed",
        ends = "last"
      ),
      show.legend = FALSE
    ) +
    geom_segment(
      data = filter(top_edges, dir < 0),
      aes(x = x0, xend = x1, y = y0, yend = y1, color = source),
      size = 0.75,
      arrow = arrow(
        length = unit(0.075, "inches"),
        type = "closed",
        ends = "first"
      ),
      show.legend = FALSE
    ) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    xlim(-1.5, 1.5) +
    ylim(-1.37, 1.37) +
    facet_wrap(~title, ncol = 5) +
    coord_fixed() +
    labs(
      title = paste("F • R • I • E • N • D • S  —  Season", season),
      subtitle = "#tidytuesday 37|2020",
      caption = "© 2020 spren9er"
    ) +
    theme_void() +
    theme(
      text = element_text(family = "Open Sans", color = "#efefef"),
      strip.text = element_text(
        size = 11, lineheight = 1.2, face = "bold", family = "Open Sans",
        color = "#efefef", margin = margin(b = 1, t = 1)
      ),
      strip.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.spacing.y = unit(1.5, "lines"),
      panel.background = element_rect(fill = "#4f5360", color = "#4f5360"),
      plot.background = element_rect(fill = "#4f5360", color = "#4f5360"),
      plot.title = element_text(
        size = 22, hjust = 0.5, margin = margin(t = 35)
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, face = "bold", margin = margin(t = 20, b = 35)
      ),
      plot.caption = element_text(
        size = 8, color = "#efefef", hjust = 0.5, face = "bold",
        margin = margin(t = 15, b = 15)
      )
    )

  if (season == 1) {
      p <- ggdraw(p) +
        draw_label(
          "For each episode the top ten interactions\nbetween main characters of FRIENDS are\ndisplayed. Bubble sizes indicate the amount\nof spoken words of a character.",
          x = 0.77,
          y = 0.1,
          hjust = 0,
          size = 8,
          fontfamily = "Open Sans",
          fontface = "bold",
          color = "#efefef",
          lineheight = 1.1
        )
  }

  ggsave(
    paste0(
      "images/tidytuesday_202037_friends_season_",
      str_pad(season, 2, pad = "0"),
      ".png"
    ),
    p,
    width = 20, height = 20, dpi = 300, bg = "#4f5360"
  )

  return(p)
}

# %% [markdown]
# Export images for all seasons

# %%
map(seq(1, 10), plot_season)
