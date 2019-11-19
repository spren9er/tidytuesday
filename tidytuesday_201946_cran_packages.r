library(tidyverse)
library(igraph)
library(ggraph)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-11-12/'
  )

data <- read_csv(paste0(path, 'loc_cran_packages.csv'))

# most popular programming languages from TIOBE Index (Nov. 2019) found in data
# (only languages with position <= 16 are considered)
popular_languages <- c(
  'Java', 'C', 'Python', 'C++', 'C#', 'Visual Basic', 'JavaScript', 'PHP', 'SQL', 'Ruby', 'Objective C++', 'Assembly', 'R'
)

# number of packages to display
number_of_pkgs <- 300

# find largest packages written in popular languages
top_packages <- data %>%
  filter(language %in% popular_languages) %>%
  group_by(pkg_name) %>%
  summarize(total_code = sum(code)) %>%
  arrange(desc(total_code)) %>%
  head(number_of_pkgs) %>%
  select(pkg_name, total_code)

# all popular languages per package
top_languages_per_pkg <- data %>%
  filter(
    pkg_name %in% top_packages$pkg_name,
    language %in% popular_languages
  ) %>%
  arrange(pkg_name, desc(code)) %>%
  group_by(pkg_name) %>%
  mutate(
    main = row_number() == 1, # main language of package should be opaque
    total_code = sum(code)
  ) %>%
  ungroup() %>%
  select(language, pkg_name, code, total_code, main)

# only following languages found in given packages
(top_languages <- top_languages_per_pkg %>%
  pull(language) %>%
  unique %>%
  sort)

top_language_colors <- c(
  '#efb306',
  '#eb990c',
  '#e8351e',
  '#cd023d',
  '#852f88',
  '#4e54ac',
  '#0f8096',
  '#7db954',
  '#17a769',
  '#000000'
)

names(top_language_colors) <- c(
  'Assembly',
  'C',
  'C++',
  'JavaScript',
  'Java',
  'R',
  'Python',
  'Ruby',
  'SQL',
  'All'
)

edges1 <- top_languages_per_pkg %>%
  transmute(from = language, to = pkg_name, total_code = code, main)

edges2 <- top_languages_per_pkg %>%
  count(language, wt = code, name = 'total_code') %>%
  transmute(
    from = '',
    to = language,
    total_code,
    main = TRUE
  )

edges <- bind_rows(edges1, edges2)

vertices1 <- top_languages_per_pkg %>%
  filter(main) %>%
  transmute(
    node = pkg_name, language, total_code, level = 1
  )

vertices2 <- edges2 %>%
  transmute(
    node = to, language = to, total_code, level = 2
  )

vertices3 <- tibble(
  node = '', language = NA, total_code = 0, level = 3
)

vertices <- bind_rows(vertices1, vertices2, vertices3) %>%
  mutate(
    radius = total_code**(1.8), # scaling circles
    language = factor(language, names(top_language_colors))
  ) %>%
  arrange(level, language, node)

graph <- graph_from_data_frame(edges, vertices = vertices)

# create custom layout by updating existing circle layout
layout <- create_layout(graph, layout = 'circle')

outer_circle <- layout %>%
  filter(level == 1) %>%
  mutate(language = factor(language, names(top_language_colors))) %>%
  arrange(language, desc(name)) %>%
  mutate(
    x = cos((row_number() - 1) / number_of_pkgs * 2 * pi),
    y = sin((row_number() - 1) / number_of_pkgs * 2 * pi)
  )

# positioning circle centers manually by specifying polar coords
angles <- c(3, 43, 119, 160, 178, 255, 350, 190, 340, 0)
radii <- c(0.8, 0.5, 0.6, 0.4, 0.65, 0.45, 0.6, 0.7, 0.38, 0)
centers <- tibble(
  x = radii * cos(angles / 180 * pi),
  y = radii * sin(angles / 180 * pi)
)
inner_circle <- bind_cols(centers, select(filter(layout, level != 1), -x, -y))

layout[] <- bind_rows(outer_circle, inner_circle) %>%
  arrange(ggraph.index)

ggraph(layout) +
  geom_edge_diagonal(
    aes(edge_color = node1.language, edge_alpha = as.factor(main)),
    edge_width = 0.3, show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = radius, color = language),
    alpha = 0.6, show.legend = FALSE
  ) +
  geom_node_text(
    aes(
      x = 1.0175 * x,
      y = 1.0175 * y,
      label = name,
      angle = -((-node_angle(x, y) + 90) %% 180) + 90,
      filter = !(name %in% top_languages)
    ),
    size = 2, hjust = 'outward', family = 'Oswald'
  ) +
  geom_node_text(
    aes(
      x = x,
      y = y,
      label = name,
      filter = name %in% top_languages
    ),
    size = 6, hjust = 0.5, family = 'Oswald'
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
      filter = name %in% top_languages
    ),
    size = 3, hjust = 0.5, family = 'Oswald'
  ) +
  scale_edge_color_manual(values = top_language_colors) +
  scale_color_manual(values = top_language_colors) +
  scale_size_area(max_size = 150) +
  scale_edge_alpha_manual(values = c(0.15, 1)) +
  coord_fixed() +
  labs(
    title = 'LOC of Popular Programming Languages in 300 CRAN Packages',
    subtitle = 'considered are largest CRAN packages written in one (or more) of top 16 programming languages from TIOBE Index (Nov. 2019)',
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
