library(tidyverse)
library(janitor)
library(rgdal)
library(ggforce)

path <-
  paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/',
    'master/data/2019/2019-05-21/'
  )

mismanaged_vs_gdp <- read_csv(
  paste0(path, 'per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv')
)

waste_vs_gdp <- read_csv(
  paste0(path, 'per-capita-plastic-waste-vs-gdp-per-capita.csv')
)

data <- mismanaged_vs_gdp %>%
  inner_join(waste_vs_gdp, by = c('Entity', 'Code', 'Year')) %>%
  clean_names() %>%
  transmute(
    country = entity,
    code,
    waste_per_day_pc =
      per_capita_plastic_waste_kilograms_per_person_per_day,
    mismanaged_waste_per_day_pc =
      per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
    mismanaged_waste_rate = mismanaged_waste_per_day_pc / waste_per_day_pc
  ) %>%
  filter(!is.na(coalesce(mismanaged_waste_per_day_pc, waste_per_day_pc)))

# naturalearthdata.com/downloads/110m-physical-vectors/110m-land
world <- readOGR(dsn = 'data/ne_110m_land', layer = 'ne_110m_land')

world_df <- fortify(world) %>%
  filter(!id %in% as.character(0:7)) # remove antarctica

# naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries
countries <- readOGR(
  dsn = 'data/ne_110m_admin_0_countries', layer = 'ne_110m_admin_0_countries'
)

countries_df <- fortify(countries) %>%
  filter(id != '159')  # remove antarctica

countries_mapping <- as.tibble(countries) %>%
  mutate(id = as.character(row_number() - 1)) %>%
  clean_names() %>%
  transmute(id, country_code = coalesce(adm0_a3, iso_a3), name_en)

centers <- countries_df %>%
  filter(
    str_detect(group, '^.*\\.1$'), # select only main part of country
  ) %>%
  group_by(id) %>%
  group_map(~ as.tibble(geosphere::centroid(select(., long, lat)))) %>%
  rename(center_long = lon, center_lat = lat)

(kpis_per_country <- data %>%
  arrange(mismanaged_waste_rate) %>%
  left_join(countries_mapping, by = c('code' = 'country_code')) %>%
  select(id, everything()) %>%
  filter(!is.na(id), id != 175)) # wrong data for Trinidad & Tobago?

kpis_per_country_gathered <- kpis_per_country %>%
  left_join(centers, by = 'id') %>%
  pivot_longer(
    c(waste_per_day_pc, mismanaged_waste_per_day_pc),
    names_to = 'waste_key', values_to = 'waste_value'
  )

selected_countries <- c(
  'KOR', 'JPN', 'AUS', 'GBR', 'QAT', 'NLD', 'PRT', 'FLK', 'GRL', 'NCL', 'GRC',
  'DEU', 'FRA', 'USA', 'CAN', 'MEX', 'BRA', 'SOM', 'PAK', 'PNG', 'SLB', 'VNM',
  'KHM', 'MMR', 'BGD', 'PRK', 'IND', 'ZAF'
)

country_labels <- kpis_per_country %>%
  mutate(
    rank = row_number(),
    name = paste0(
      rank, '. ', name_en,
      ' (', as.character(scales::percent_format()(mismanaged_waste_rate)), ')'
    )
  ) %>%
  left_join(centers, by = 'id') %>%
  select(code, center_long, center_lat, name, mismanaged_waste_rate) %>%
  filter(
    code %in% selected_countries
  ) %>%
  mutate(code = factor(code, levels = selected_countries))

theme_opts <- list(theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  plot.background = element_blank(),
  panel.border = element_blank(),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
))

ggplot() +
  geom_polygon(
    data = world_df,
    aes(x = long, y = lat, group = group, fill = hole)) +
  geom_path(
    data = countries_df,
    aes(x = long, y = lat, group = group, fill = hole),
    size = 0.1, color = '#cccccc'
  ) +
  geom_point(
    data = kpis_per_country_gathered,
    aes(
      x = center_long, y = center_lat,
      size = waste_value, color = waste_key
    )
  ) +
  geom_mark_circle(
    data = country_labels,
    aes(x = center_long, y = center_lat, group = name, label = name),
    fill = 'transparent', size = 0,
    label.fill = 'transparent', label.fontsize = 4,
    con.size = 0.05, con.cap = 0, expand = unit(0, 'mm')
  ) +
  scale_fill_manual(values = c('#dedede', '#ffffff'), guide = 'none') +
  scale_color_manual(
    labels = c(
      'mismanaged plastic waste per capita (in kg per day)',
      'plastic waste per capita (in kg per day)'
    )
  ) +
  scale_size_area(max_size = 6, breaks = c(0.1, 0.2, 0.4, 0.6)) +
  coord_equal() +
  theme_opts +
  labs(
    color = 'Color',
    size = 'Size',
    title = 'Global Plastic Waste 2010',
    subtitle = paste(
      'Mismanaged Plastic Waste Rates per Country',
      '#tidytuesday 21|2019',
      sep = '  •  '
    ),
    caption = '© 2019 spren9er'
  )

ggsave(
  'images/tidytuesday_201921_global_plastic_waste.png',
  width = 13, height = 8, dpi = 300
)
