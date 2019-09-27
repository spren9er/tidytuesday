library(tidyverse)
library(janitor)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-09-24/'
  )

data <- read_csv(paste0(path, 'school_diversity.csv')) %>%
  clean_names()

data <- data %>%
  select(leaid, total, school_year, diverse) %>%
  rename(id = leaid) %>%
  mutate(
    school_year = str_sub(school_year, 1, 4),
    diverse = fct_recode(
      diverse,
      '1' = 'Diverse', '0' = 'Undiverse', '-1' = 'Extremely undiverse'
    )
  ) %>%
  pivot_wider(names_from = school_year, values_from = c(diverse, total)) %>%
  drop_na()

nrow(data)

# raw data -> d3 (use d3.force layout to calculate source and target coords)
write_csv(data, 'data/tidytuesday_201939_school_diversity_raw.csv')

################################################################################

# combine source and target coords to one single data frame
data_source <- read_csv(
  'data/tidytuesday_201939_school_diversity_preprocessed_source.csv'
)

data_target <- read_csv(
  'data/tidytuesday_201939_school_diversity_preprocessed_target.csv'
)

data_source <- data_source %>%
  mutate(sourceX = x, sourceY = y) %>%
  select(id, x, y, color, sourceRadius, targetRadius, sourceX, sourceY)

data_target <- data_target %>%
  mutate(targetX = x, targetY = y) %>%
  select(id, targetX, targetY)

data_source %>%
  inner_join(data_target) %>%
  write_csv('data/tidytuesday_201939_school_diversity_preprocessed.csv')

################################################################################

# export d3 animation to video (.mov) using quick time player screen recording
# and ffmpeg to change frame rate
# ffmpeg -y -i input.mov -vf "setpts=0.1*PTS" output.mov
