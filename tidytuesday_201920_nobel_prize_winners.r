library(tidyverse)
library(lubridate)
library(ggrepel)

nobel_winners <- read_csv(
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-05-14/nobel_winners.csv'
  )
)

dead_nobel_winners <- nobel_winners %>%
  filter(!is.na(death_date)) %>%
  mutate(years_until_death = year(death_date) - prize_year) %>%
  distinct(category, full_name, years_until_death)

avg_years_until_death <- dead_nobel_winners %>%
  group_by(category) %>%
  summarize(avg_years_until_death = mean(years_until_death))

max_years_until_death <- dead_nobel_winners %>%
  group_by(category) %>%
  top_n(1, years_until_death)

min_years_until_death <- dead_nobel_winners %>%
  group_by(category) %>%
  top_n(1, -years_until_death)

dead_nobel_winners %>%
  left_join(avg_years_until_death, by = 'category') %>%
  anti_join(min_years_until_death) %>%
  anti_join(max_years_until_death) %>%
  mutate(category = fct_reorder(category, avg_years_until_death)) %>%
  ggplot() +
    geom_vline(aes(xintercept = 0)) +
    geom_jitter(
      aes(x = -years_until_death, y = category),
      color = 'red', alpha = 1/3, height = 0.15, width = 0
    ) +
    geom_line(
      data = avg_years_until_death,
      aes(x = -avg_years_until_death, y = category, group = 1)
    ) +
    geom_point(
      data = avg_years_until_death,
      aes(x = -avg_years_until_death, y = category)
    ) +
    geom_point(
      data = max_years_until_death,
      aes(x = -years_until_death, y = category),
      color = 'red', alpha = 1/3
    ) +
    geom_label_repel(
      data = max_years_until_death,
      aes(x = -years_until_death, y = category, label = full_name),
      hjust = 1, size = 2.25, nudge_y = -0.4, nudge_x = 0.8,
      segment.size = 0.3, segment.alpha = 0.9, label.size = 0.25
    ) +
    geom_point(
      data = min_years_until_death,
      aes(x = -years_until_death, y = category),
      color = 'red', alpha = 1/3
    ) +
    geom_label_repel(
      data = min_years_until_death,
      aes(x = -years_until_death, y = category, label = full_name),
      hjust = 1, size = 2.25, nudge_y = -0.4, nudge_x = -0.8,
      segment.size = 0.3, segment.alpha = 0.9, label.size = 0.25
    ) +
    scale_x_continuous(
      breaks = seq(-60, 0, by = 5), labels = function(l) {-l}
    ) +
    labs(
      x = 'Number of Years',
      y = 'Category',
      title = 'Number of Years between Nobel Prize and Death',
      subtitle = paste(
        'Distribution and Average',
        '#tidytuesday 20|2019',
        sep = ' • '
      ),
      caption = '© 2019 spren9er'
    )

ggsave(
  'images/tidytuesday_201920_nobel_prize_winners.png',
  width = 6.5, height = 6.5, bg = 'transparent'
)
