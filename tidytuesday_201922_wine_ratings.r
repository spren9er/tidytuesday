library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(viridis)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-05-28/'
  )

wine_ratings <- read_csv(paste0(path, 'winemag-data-130k-v2.csv')) %>%
  rename(id = X1)

# common words to describe wine: https://www.words-to-use.com/words/wine/
wine_adjectives <- read_csv2('data/wine_adjectives.csv', col_names = c('word'))

theme_options <- theme(
  legend.title = element_text(
    size = 9, margin = margin(b = 5), face = 'bold'
  ),
  legend.text = element_text(size = 7),
  legend.margin = margin(t = 15, b = 15),
  legend.key.width = unit(10, 'points'),
  plot.title = element_text(
    margin = margin(b = 12), color = '#32b37f', size = 14, hjust = 0.5,
    face = 'bold'
  ),
  plot.subtitle = element_text(
    margin = margin(b = 15), size = 11, hjust = 0.5, face = 'bold'
  ),
  plot.caption = element_text(color = '#dadada', size = 6, hjust = 1.09),
  plot.margin = margin(t = 40, r = 20, b = 20, l = 20)
)

wine_words <- wine_ratings %>%
  unnest_tokens('word', description) %>%
  filter(word %in% wine_adjectives$word) %>%
  select(id, country, points, price, variety, word)

top_wine_words <- wine_words %>%
  group_by(word) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  head(120)

threshold <- 0.02

wine_word_correlations <- wine_words %>%
  filter(word %in% top_wine_words$word) %>%
  pairwise_cor(word, id, sort = TRUE) %>%
  filter(correlation > threshold) %>%
  arrange(desc(correlation))

wine_averages_per_word <- wine_words %>%
  filter(word %in% top_wine_words$word) %>%
  group_by(word) %>%
  summarize(
    total = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE)
  ) %>%
  rename(name = word) %>%
  arrange(desc(total))

graph <- wine_word_correlations %>%
  rename(weight = correlation) %>%
  mutate(alpha = cut(weight, c(threshold, 0.05, 1))) %>%
  graph_from_data_frame(vertices = wine_averages_per_word)

ggraph(graph, layout = 'fr', niter = 15000) +
  geom_edge_link(aes(edge_alpha = alpha), edge_width = 0.2) +
  geom_node_point(aes(size = total, color = avg_points)) +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  scale_color_viridis(
    limits = c(84, 91.5),
    breaks = c(85.0, 87.5, 90.0, 92.5)
  ) +
  scale_size_area(
    breaks = c(250, 1000, 2500, 5000, 10000, 25000),
    labels = function(n) { format(n, big.mark = ',') }
  ) +
  scale_edge_alpha_manual(
    values = c(0.03, 0.4), labels = c('weak', 'strong')
  ) +
  labs(
    title = paste(
      'Words in',
      format(nrow(wine_ratings), big.mark = ','),
      'Wine Descriptions I.'
    ),
    subtitle = paste(
      '120 common words to describe wine and their correlation',
      '#tidytuesday 22 | 2019',
      sep = '   •   '
    ),
    caption = '© 2019 spren9er',
    color = 'Average Rating',
    size = 'Word Count',
    edge_alpha = 'Correlation'
  ) +
  theme_void() +
  theme_options +
  guides(
    edge_alpha = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

ggsave(
  'images/tidytuesday_201922_wine_ratings_most_common_words.png',
  width = 10, height = 8.5, dpi = 300
)

################################################################################

wine_words <- wine_ratings %>%
  unnest_tokens('word', description) %>%
  anti_join(stop_words, by = 'word') %>%
  filter(
    !str_detect(word, '^\\d+$'),
    !word %in% c('alongside', 'offers', 'feels')
  ) %>%
  select(id, country, points, price, variety, word)

top_wine_words <- wine_words %>%
  count(word, sort = TRUE) %>%
  head(150)

wine_word_correlations <- wine_words %>%
  filter(word %in% top_wine_words$word) %>%
  pairwise_cor(word, id, sort = TRUE) %>%
  filter(correlation > 0.0) %>%
  arrange(desc(correlation))

wine_averages_per_word <- wine_words %>%
  filter(word %in% top_wine_words$word) %>%
  group_by(word) %>%
  summarize(
    total = n(),
    avg_points = mean(points, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE)
  ) %>%
  rename(name = word) %>%
  arrange(desc(total))

threshold <- 0.065

graph <- wine_word_correlations %>%
  filter(correlation > threshold) %>%
  rename(weight = correlation) %>%
  mutate(alpha = cut(weight, c(0, threshold, 0.13, 1))) %>%
  graph_from_data_frame(vertices = wine_averages_per_word)

ggraph(graph, layout = 'fr', niter = 15000) +
  geom_edge_link(aes(edge_alpha = alpha), edge_width = 0.2) +
  geom_node_point(aes(size = total, color = avg_points)) +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  scale_color_viridis(
    limits = c(86, 90), breaks = c(86.0, 87.0, 88.0, 89.0, 90.0)
  ) +
  scale_size_area(
    breaks = c(5000, 10000, 25000, 50000),
    labels = function(n) { format(n, big.mark = ',') }
  ) +
  scale_edge_alpha_manual(
    values = c(0.03, 0.4), labels = c('weak', 'strong')
  ) +
  labs(
    title = paste(
      'Words in',
      format(nrow(wine_ratings), big.mark = ','),
      'Wine Descriptions II.'
    ),
    subtitle = paste(
      '150 most frequent words and their correlation',
      '#tidytuesday 22 | 2019',
      sep = '   •   '
    ),
    caption = '© 2019 spren9er',
    color = 'Average Rating',
    size = 'Word Count',
    edge_alpha = 'Correlation'
  ) +
  theme_void() +
  theme_options +
  guides(
    edge_alpha = guide_legend(order = 1),
    size = guide_legend(order = 2)
  )

ggsave(
  'images/tidytuesday_201922_wine_ratings_most_frequent_words.png',
  width = 10, height = 8.5, dpi = 300
)
