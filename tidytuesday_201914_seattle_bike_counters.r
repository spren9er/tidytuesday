library(tidyverse)
library(lubridate)
library(viridis)

options(lubridate.week.start = 1)

raw_data <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
  col_types = list(
    date = col_datetime('%m/%d/%Y %H:%M:%S %p'),
    bike_count = col_integer(),
    ped_count = col_integer()
  )
)

bike_traffic <- raw_data %>%
  filter(!is.na(bike_count), bike_count < 2000, date >= ymd(20140101))

bike_traffic_totals <- bike_traffic %>%
  mutate(wday = wday(date, label = TRUE), hour = hour(date)) %>%
  group_by(crossing, direction, wday, hour) %>%
  summarize(total = sum(bike_count)) %>%
  group_by(crossing, direction) %>%
  mutate(
    percentage = total / max(total) * 100,
    name = paste0(crossing, ' (', direction, ')')
  )

levels <- c(
  '39th Ave NE Greenway at NE 62nd St (North)',
  '39th Ave NE Greenway at NE 62nd St (South)',
  'Broadway Cycle Track North Of E Union St (North)',
  'Broadway Cycle Track North Of E Union St (South)',
  'Burke Gilman Trail (North)',
  'Burke Gilman Trail (South)',
  'Elliot Bay Trail (North)',
  'Elliot Bay Trail (South)',
  'NW 58th St Greenway at 22nd Ave (West)',
  'NW 58th St Greenway at 22nd Ave (East)',
  'Sealth Trail (North)',
  'Sealth Trail (South)',
  'MTS Trail (East)'
)

bike_traffic_totals %>%
  mutate(name = factor(name, levels = levels)) %>%
  ggplot() +
    geom_tile(aes(x = hour, y = wday, fill = percentage)) +
    scale_x_continuous(breaks = 0:23) +
    scale_fill_viridis(breaks = c(0, 100), labels = c('low', 'high')) +
    facet_wrap(~ name, ncol = 2, scales = 'free') +
    labs(
      x = 'Time of Day',
      y = 'Weekday',
      fill = '',
      title = "Seattle's Bike Counts per Time of Day and Weekday 2014-2019",
      subtitle = '#tidytuesday 14|2019',
      caption = '© 2019 spren9er'
    )

ggsave(
  'images/tidytuesday_201914_seattle_bike_counters.png',
  width = 10, height = 13, bg = 'transparent'
)
