library(tidyverse)

raw_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

women_research <- raw_data %>%
  mutate(
    field = gsub('Women Inventores', 'Inventors', str_to_title(field)),
    percent_men = 1 - percent_women,
    women_ratio = ifelse(percent_women < percent_men, 'less', 'not_less')
  ) %>%
  pivot_longer(
    starts_with('percent'), names_to = 'sex', values_to = 'percent',
    names_prefix = 'percent_'
  )

women_field_averages <- women_research %>%
  filter(sex == 'women') %>%
  group_by(field) %>%
  summarize(avg_field_percent = mean(percent)) %>%
  arrange(avg_field_percent)

women_country_averages <- women_research %>%
  filter(sex == 'women') %>%
  group_by(country) %>%
  summarize(avg_country_percent = mean(percent)) %>%
  arrange(avg_country_percent)

plot <- women_research %>%
  mutate(
    field = factor(field, levels = women_field_averages$field),
    country = factor(country, levels = women_country_averages$country),
    sex_women_ratio = interaction(sex, women_ratio)
  ) %>%
  ggplot() +
    geom_bar(
      aes(x = '', y = percent, fill = sex_women_ratio, color = women_ratio),
      stat = 'identity', show.legend = FALSE
    ) +
    scale_fill_manual(values = c('#efefef', '#333333', '#efefef', '#c54950')) +
    scale_color_manual(values = c('#333333', '#c54950')) +
    coord_polar('y', start = 0) +
    facet_grid(field ~ country, switch = 'y') +
    labs(
      x = '',
      y = '',
      title = "Still a man's world",
      subtitle = '#tidytuesday 16|2019 • women among researchers with papers published 2011-2015',
      caption = '© 2019 spren9er'
    )

plot +
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.9, 'lines'),
    panel.spacing.y = unit(0.9, 'lines'),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, face = 'plain'),
    strip.text.x = element_text(angle = 90, hjust = 0, margin = margin(b = 10)),
    strip.text.y = element_text(
      angle = 180, hjust = 1, margin = margin(r = 10)
    ),
    plot.title = element_text(face = 'bold', margin = margin(b = 7)),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.caption = element_text(
      color = '#333333', face = 'plain', size = 7, hjust = 1,
      margin = margin(t = 20)
    )
  )

ggsave(
  'images/tidytuesday_201916_the_economist.png',
  width = 9.5, height = 6.5, bg = '#efefef'
)
