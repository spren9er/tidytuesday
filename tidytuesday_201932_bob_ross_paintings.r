library(tidyverse)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-08-06/'
  )

data <- read_csv(paste0(path, 'bob-ross.csv')) %>%
  janitor::clean_names() %>%
  mutate(
    episode = str_replace(episode, 'S0', 'S'),
    episode = str_replace(episode, 'E0', 'E'),
    title = str_to_title(str_replace_all(title, '"', ''))
  ) %>%
  select(-contains('STEVE'), -contains('DIANE'), -contains('FRAME'))

colors <- read_csv(
    'data/tidytuesday_201932_bob_ross_paintings.csv',
    col_names = c('episode', 'title', 'color', 'color_name')
  ) %>%
  filter(!color %in% c('#FFFFFF', '#000000'))

ordered_colors <- c(
  'Phthalo Blue', 'Prussian Blue', 'Phthalo Green', 'Sap Green',
  'Van Dyke Brown', 'Alizarin Crimson', 'Dark Sienna', 'Burnt Umber',
  'Bright Red', 'Indian Red', 'Yellow Ochre', 'Indian Yellow', 'Cadmium Yellow'
)

palette <- colors %>%
  distinct(color, color_name) %>%
  mutate(color_name = fct_relevel(color_name, ordered_colors)) %>%
  arrange(color_name)

objects <- data %>%
  pivot_longer(
    -one_of('episode', 'title'),
    names_to = 'object', values_to = 'painted'
  ) %>%
  mutate(object = str_to_title(str_replace_all(object, '_', ' '))) %>%
  filter(object != 'Guest')

top_objects <- objects %>%
  filter(painted == 1) %>%
  count(object, name = 'object_total', sort = TRUE) %>%
  filter(object_total >= 10) %>%
  mutate(object = fct_reorder(object, -object_total))

objects_colors <- objects %>%
  right_join(top_objects, by = 'object') %>%
  inner_join(select(colors, -title), by = 'episode') %>%
  count(object, color_name, painted, name = 'total', sort = TRUE) %>%
  filter(painted == 1) %>%
  select(-painted) %>%
  pivot_wider(names_from = color_name, values_from = total) %>%
  replace(is.na(.), 0)

objects_colors_mtx <- as.matrix(column_to_rownames(objects_colors, 'object'))
chi2 <- chisq.test(objects_colors_mtx, correct = F)
residuals <- as.tibble(chi2$residuals, rownames = 'object')

cut_residuals <- residuals %>%
  pivot_longer(-object, names_to = 'color', values_to = 'residual') %>%
  mutate(
    cut_residual = ifelse(residual > 1, 1, residual),
    cut_residual = ifelse(residual < 0, 0, cut_residual)
  )

dist_mtx <- cut_residuals %>%
  pivot_wider(object, names_from = color, values_from = cut_residual) %>%
  column_to_rownames('object') %>%
  as.matrix() %>%
  dist()

cluster <- hclust(dist_mtx)

reordered_objects <- cluster$labels[cluster$order]

cut_residuals %>%
  mutate(
    color = fct_relevel(color, as.character(palette$color_name)),
    object = fct_relevel(object, reordered_objects)
  ) %>%
  filter(cut_residual > 0) %>%
  ggplot(aes(x = color, y = object, fill = color, alpha = cut_residual)) +
    geom_tile(width = 0.9, height = 0.9, show.legend = FALSE) +
    scale_fill_manual(values = palette$color) +
    scale_x_discrete(position = 'top') +
    scale_alpha_continuous(range = c(0, 1)) +
    labs(
      x = '',
      y = '',
      title = 'Elements & Colors in Bob Ross Paintings',
      subtitle = '#tidytuesday 32 | 2019',
      caption = '© 2019 spren9er'
    ) +
    theme(
      text = element_text(family = 'Dosis'),
      axis.text.x = element_text(size = 6.5, angle = 90, hjust = 0),
      axis.text.y = element_text(size = 6.5),
      axis.text.x.top = element_text(vjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 8, hjust = -0.53, margin = margin(t = 10, b = 5),
        color = '#555555'
      ),
      plot.subtitle = element_text(
        size = 7, hjust = -0.28, face = 'plain', color = '#555555'
      ),
      plot.caption = element_text(
        color = '#cccccc', size = 5, margin = margin(t = -6), hjust = 0.9825,
        face = 'plain'
      ),
      plot.margin = margin(t = 10, r = 10, b = 10, l = -6)
    )

ggsave(
  'images/tidytuesday_201932_bob_ross_paintings.png',
  width = 3.8, height = 8, dpi = 300
)
