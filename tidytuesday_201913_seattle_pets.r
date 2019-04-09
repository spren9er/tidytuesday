library(tidyverse)
library(lubridate)
library(broom)
library(ggrepel)

raw_data <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv'
)

pets_totals <- raw_data %>%
  mutate(
    year = year(mdy(license_issue_date)),
    animals_name = str_to_title(animals_name)
  ) %>%
  filter(species %in% c('Cat', 'Dog'), !is.na(animals_name), year >= 2015) %>%
  count(species, animals_name) %>%
  pivot_wider(
    names_from = species, values_from = n, values_fill = list(n = 0)
  ) %>%
  transmute(animal_name = animals_name, dog = Dog, cat = Cat)

model <- lm(cat ~ dog, pets_totals)
intercept <- model$coefficients[1]
slope <- model$coefficients[2]

pets_popular <- augment(model, pets_totals) %>%
  mutate(
    total = dog + cat,
    most_popular = total >= 230,
    popular = total >= 70,
    sign = factor(-sign(.resid))
  )

pets_popular %>%
  ggplot(aes(x = dog, y = cat)) +
    geom_abline(
      intercept = intercept, slope = slope, size = 0.25, linetype = 2
    ) +
    geom_text_repel(
      data = filter(pets_popular, popular & !most_popular),
      aes(label = animal_name, size = total, color = sign),
      fontface = 'bold', segment.size = 0.25, segment.alpha = 0.35, seed = 6,
      show.legend = FALSE
    ) +
    geom_text(
      data = filter(pets_popular, most_popular),
      aes(label = animal_name, size = total, color = sign),
      fontface = 'bold', show.legend = FALSE
    ) +
    geom_label_repel(
      data = filter(pets_popular, most_popular),
      aes(label = paste0(total, ' (', dog, '/', cat, ')')),
      fontface = 'bold', label.padding = 0.2, size = 2, nudge_y = -3.5,
      show.legend = FALSE
    ) +
    expand_limits(x = 0, y = 0) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    scale_size_continuous(range = c(1.5, 4.25)) +
    labs(
      x = 'Number of Dogs',
      y = 'Number of Cats',
      title = "Seattle's Most Popular Dog and Cat Names 2015-2018",
      subtitle = '#tidytuesday 13|2019',
      caption = '© 2019 spren9er'
    )

ggsave(
  'images/tidytuesday_201913_seattle_pets.png', 
  dpi = 600, bg = 'transparent'
)
