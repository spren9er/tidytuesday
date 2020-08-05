library(tidyverse)

data <- tidytuesdayR::tt_load(2020, week = 32)

energy <- data$energy_types %>%
  mutate(
    type = fct_other(
      type,
      keep = c("Nuclear", "Conventional thermal"),
      other_level="Renewable"
    ),
    type = fct_relevel(type, "Renewable", "Nuclear", "Conventional thermal")
  ) %>%
  pivot_longer(
    c("2016", "2017", "2018"),
    names_to = "year",
    values_to = "total"
  ) %>%
  select(-level) %>%
  group_by(country_name, country, type, year) %>%
  summarize(total = sum(total)) %>%
  ungroup()

countries_order <- energy %>%
  mutate(clean_electricity = (type == "Renewable")) %>%
  group_by(country, clean_electricity) %>%
  summarize(total = sum(total)) %>%
  pivot_wider(names_from = clean_electricity, values_from = total) %>%
  mutate(ratio = `TRUE` / (`TRUE` + `FALSE`)) %>%
  arrange(desc(ratio)) %>%
  pull(country)

energy <- energy %>%
  mutate(
    country = fct_relevel(country, countries_order),
    year = fct_relevel(as.character(year), "2018", "2017", "2016")
  )

color <- function(type, year) {
  case_when(
    type == "Conventional thermal" && year == 2016 ~ "#cf5c4f",
    type == "Conventional thermal" && year == 2017 ~ "#ca4d3f",
    type == "Conventional thermal" && year == 2018 ~ "#c04335",
    type == "Nuclear" && year == 2016 ~ "#f3e8e2",
    type == "Nuclear" && year == 2017 ~ "#edddd4",
    type == "Nuclear" && year == 2018 ~ "#e7d1c5",
    type == "Renewable" && year == 2016 ~ "#1f9098",
    type == "Renewable" && year == 2017 ~ "#1c8087",
    type == "Renewable" && year == 2018 ~ "#187077",
    TRUE ~ "#333333"
  )
}

create_marimekko_data <- function(df, ...) {
  columns <- enquos(...)

  marimekko <- df %>%
    mutate(overall_total = sum(total)) %>%
    arrange(!!!columns)

  for (i in seq(length(columns))) {
    x_total <- rlang::sym(paste0("x", i, "_total"))
    x_pct <- rlang::sym(paste0("x", i, "_pct"))
    x_total_prev <- ifelse(
      i == 1,
      rlang::sym("overall_total"),
      rlang::sym(paste0("x", i - 1, "_total"))
    )

    marimekko <- marimekko %>%
      group_by(!!!columns[1:i]) %>%
      mutate(
        !!x_total := sum(total),
        !!x_pct := !!x_total / !!x_total_prev
      )

    x_min_pct <- rlang::sym(paste0("x", i, "_min_pct"))
    x_max_pct <- rlang::sym(paste0("x", i, "_max_pct"))

    cum_x <- marimekko %>%
      group_by(!!!columns[1:i]) %>%
      summarize(!!x_pct := first(!!x_pct)) %>%
      mutate(
        !!x_max_pct := cumsum(!!x_pct),
        !!x_min_pct := lag(!!x_max_pct, default = 0)
      ) %>%
      select(-!!x_pct)

    marimekko <- inner_join(marimekko, cum_x)
  }

  labels <- marimekko %>%
    group_by(!!!columns[1], x1_max_pct, x1_min_pct) %>%
    summarize(label = first(!!!columns[1])) %>%
    ungroup() %>%
    transmute(ylabel = (x1_max_pct - x1_min_pct) / 2 + x1_min_pct, label)

  data <- marimekko %>%
    arrange(!!!columns) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      xmin = x2_min_pct,
      xmax = x2_max_pct,
      ymin = x1_min_pct + (x1_max_pct - x1_min_pct) * x3_min_pct,
      ymax = x1_min_pct + (x1_max_pct - x1_min_pct) * x3_max_pct,
      color = color(type, year)
    ) %>%
    select(country, country_name, type, year, xmin, xmax, ymin, ymax, color)

  out <- list()
  out$data <- data
  out$labels <- labels
  out
}

marimekko <- list(
  create_marimekko_data(energy, year, country, type),
  create_marimekko_data(energy, year, type, country),
  create_marimekko_data(energy, type, country, year)
)

marimekko_data <- bind_rows(imap(marimekko, ~ mutate(.x$data, chunk_id = .y)))

marimekko_data %>%
  filter(chunk_id == 1) %>%
  ggplot() +
    geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = interaction(type, year),
        alpha = 0.1
      ),
      color = "#333333",
      size = 0.1,
      show.legend = FALSE
    ) +
    coord_fixed()

marimekko_data %>%
  select(chunk_id, year, country, type, xmin, xmax, ymin, ymax, color) %>%
  arrange(chunk_id, year, country, type) %>%
  replace(is.na(.), 0) %>%
  write_csv(
    "data/tidytuesday_202032_european_energy.csv"
  )

marimekko_labels <- bind_rows(
  imap(marimekko, ~ mutate(.x$labels, chunk_id = .y))
)

blank_countries <- energy %>%
  group_by(country) %>%
  summarize(total = sum(total)) %>%
  arrange(total) %>%
  head(22) %>%
  pull(country)

marimekko_labels %>%
  filter(!label %in% blank_countries) %>%
  mutate(
    label = ifelse(label == "Conventional thermal", "Conv. Thermal", label)
  ) %>%
  write_csv(
    "data/tidytuesday_202032_european_energy_labels.csv"
  )
