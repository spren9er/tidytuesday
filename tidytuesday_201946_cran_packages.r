library(tidyverse)
library(igraph)
library(ggraph)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-11-12/'
  )

data <- read_csv(paste0(path, 'loc_cran_packages.csv'))

# popular programming languages from Tiobe Index (Nov. 2019)
popular_languages <- c(
  'Java', 'C', 'Python', 'C++', 'C#', 'Visual Basic', 'JavaScript', 'PHP', 'SQL', 'Ruby', 'Objective C++', 'Assembly', 'R'
)

number_of_pkgs <- 300

top_language_colors <- list(
  'Assembly'   = '#efb306',
  'C'          = '#eb990c',
  'C++'        = '#e8351e',
  'JavaScript' = '#852f88',
  'Java'       = '#cd023d',
  'R'          = '#7db954',
  'Python'     = '#0f8096',
  'Ruby'       = '#4e54ac',
  'SQL'        = '#17a769',
  'All'        = '#000000'
)

colors <- as.vector(unlist(top_language_colors))
levels <- names(top_language_colors)

top_packages <- data %>%
  filter(language %in% popular_languages) %>%
  group_by(pkg_name) %>%
  summarize(total_code = sum(code)) %>%
  arrange(desc(total_code)) %>%
  head(number_of_pkgs) %>%
  select(pkg_name, total_code)

top_languages_per_pkg <- data %>%
  filter(
    pkg_name %in% top_packages$pkg_name,
    language %in% popular_languages
  ) %>%
  left_join(top_packages, by = 'pkg_name') %>%
  mutate(language = factor(language, levels = levels)) %>%
  arrange(language, pkg_name)

top_languages <- top_languages_per_pkg %>%
  group_by(language) %>%
  summarize(total_code = sum(code)) %>%
  arrange(language) %>%
  ungroup()

edges1 <- top_languages_per_pkg %>%
  transmute(from = language, to = pkg_name, total_code = code)

edges2 <- top_languages %>%
  transmute(
    from = 'All',
    to = language,
    total_code,
    alpha = 1
  )

vertices1 <- edges1 %>%
  group_by(to) %>%
  top_n(1, total_code) %>%
  ungroup() %>%
  transmute(to, label = to, code = total_code, color = from) %>%
  left_join(
    count(edges1, to, wt = total_code, name = 'total_code'), by = 'to'
  ) %>%
  rename(node = to) %>%
  mutate(level = 1, color = factor(color, levels = levels)) %>%
  arrange(color, node)

edges1 <- left_join(
  edges1,
  edges1 %>%
    left_join(vertices1, by = c('to' = 'node', 'from' = 'color')) %>%
    transmute(from, to, alpha = level) %>%
    replace_na(list(alpha = 0))
)

edges <- bind_rows(edges1, edges2)

vertices2 <- edges2 %>%
  transmute(
    node = to, label = to, code = total_code, color = to, total_code, level = 2
  ) %>%
  arrange(node)

vertices3 = tibble(
  node = 'All', label = '', code = 0, color = NA, total_code = 0, level = 3
)

vertices = bind_rows(vertices1, vertices2, vertices3) %>%
  mutate(radius = total_code**(1.8))

graph <- graph_from_data_frame(edges, vertices = vertices)

# create custom layout by updating circle layout
layout <- create_layout(graph, layout = 'circle')

layout1 <- layout %>%
  filter(level == 1) %>%
  mutate(color = factor(color, levels = levels)) %>%
  arrange(color, desc(label)) %>%
  mutate(
    x = cos((row_number() - 1) / number_of_pkgs * 2 * pi),
    y = sin((row_number() - 1) / number_of_pkgs * 2 * pi)
  )

angles <- c(3, 43, 119, 160, 178, 255, 350, 190, 340, 0)
radii <- c(0.8, 0.5, 0.6, 0.4, 0.65, 0.45, 0.6, 0.7, 0.38, 0)
centers <- tibble(
  x = radii * map_dbl(angles, ~ cos(. / 180 * pi)),
  y = radii * map_dbl(angles, ~ sin(. / 180 * pi))
)

layout2 <- bind_cols(centers, select(filter(layout, level != 1), -x, -y))

layout[] <- bind_rows(layout1, layout2) %>%
  arrange(ggraph.index)

ggraph(layout, circular = TRUE) +
  geom_edge_diagonal(
    aes(edge_color = node1.color, edge_alpha = as.factor(alpha)),
    edge_width = 0.3, show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = radius, color = color),
    alpha = 0.6, show.legend = FALSE
  ) +
  geom_node_text(
    aes(
      x = 1.0175 * x,
      y = 1.0175 * y,
      label = label,
      angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      filter = !(label %in% top_languages$language)
    ),
    size = 2, hjust = 'outward', family = 'Oswald'
  ) +
  geom_node_text(
    aes(
      x = x,
      y = y,
      label = label,
      filter = label %in% top_languages$language
    ),
    size = 6, hjust = 0.5, family = 'Oswald',
    point.padding = NA, repel = TRUE
  ) +
  geom_node_text(
    aes(
      x = x,
      y = y - 0.045,
      label = ifelse(
        total_code > 1000,
        format(total_code, big.mark = ','),
        total_code
      ),
      filter = label %in% top_languages$language
    ),
    size = 3, hjust = 0.5, family = 'Oswald',
    point.padding = NA, repel = TRUE
  ) +
  scale_edge_color_manual(values = colors, guide = FALSE) +
  scale_color_manual(values = colors, guide = FALSE) +
  scale_size_area(max_size = 150, guide = FALSE) +
  scale_edge_alpha_manual(values = c(0.15, 1), guide = FALSE) +
  coord_fixed() +
  labs(
    title = 'LOC of Popular Programming Languages in 300 CRAN Packages',
    subtitle = 'considered are largest CRAN packages written in one (or more) of top 16 programming languages from Tiobe Index (Nov. 2019)',
    caption = '#tidytuesday 46|2019 spren9er'
  ) +
  theme_void() +
  theme(
    text = element_text(family = 'Oswald'),
    legend.position = c(0.645, 0.51),
    plot.title = element_text(
      face = 'bold', hjust = 0.5, size = 20, margin = margin(t = 45, b = 3)
    ),
    plot.subtitle = element_text(
      face = 'plain', hjust = 0.5, size = 13, margin = margin(t = 5, b = 3)),
    plot.caption = element_text(
      face = 'plain', color = '#dedede', size = 8, hjust = 1,
      margin = margin(b = 20)
    )
  )

ggsave(
  'images/tidytuesday_201946_cran_packages.png',
  width = 12, height = 12.5, dpi = 300
)
