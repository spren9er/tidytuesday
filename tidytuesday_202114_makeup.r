# %%
library(tidyverse)
library(ggforce)
library(ggrepel)
library(colorspace)
library(cowplot)

# %%
data <- tidytuesdayR::tt_load(2021, week = 14)

# %%
shades <- data$allShades
categories <- data$allCategories

# %% [markdown]
# Preprocessing

# %%
uniq_shades <- shades %>%
  distinct(brand, product, specific, lightness, .keep_all = TRUE) %>%
  select(brand, product, name, specific, hex, hue, sat, lightness)

# %%
uniq_categories <- categories %>%
  distinct(brand, product, specific, lightness, .keep_all = TRUE) %>%
  select(brand, product, name, specific, lightness, categories) %>%
  rename(c("terms" = "name"))

# %%
df <- inner_join(
  uniq_shades,
  uniq_categories,
  by = c("brand", "product", "specific", "lightness")
)

# %%
df <- separate_rows(df, categories) %>%
  mutate(terms = str_to_title(terms))


# %% [markdown]
# Convert (saturation, lightness) from square into triangle domain (HSL space)

# %%
transform <- Vectorize(function(saturation, lightness) {
  h <- 1 / tan(pi / 3)
  y <- 2 * h * lightness - h

  if (lightness > 0.5) {
    c((-2 * lightness + 2) * saturation, y)
  } else {
    c(2 * lightness * saturation, y)
  }
})

# %%
df <- df %>%
  mutate(
    transform_sat = transform(sat, lightness)[1, ],
    transform_lightness = transform(sat, lightness)[2, ]
  )

# %% [markdown]
# Calculate average color (in LAB or HSL color space)

# %%
RGB2hex <- function(r, g, b) {
  sprintf("#%s", paste(as.hexmode(c(r, g, b)), collapse = ""))
}

# %%
LAB2hex <- function(lab_color) {
  do.call(RGB2hex, as.list(round(coords(as(lab_color, "RGB")) * 255)))
}

# %%
hex2HLS <- function(hex_color) {
  coords(as(hex2RGB(hex_color), "HLS"))
}

# %%
hex2lightness <- function(hex_color) {
  hex2HLS(hex_color)[2]
}

# %%
hex2sat <- function(hex_color) {
  hex2HLS(hex_color)[3]
}

# %%
hex2LAB <- function(hex_color) {
  rgb <- hex2RGB(hex_color)
  coords(as(rgb, "LAB"))
}

# %%
avg_color_lab <- function(hex_color) {
  df <- as_tibble(hex2LAB(hex_color))
  lab_color <- do.call(LAB, unname(as.list(summarize_all(df, mean))))
  LAB2hex(lab_color)
}

# %%
avg_color_hsl <- function(hex_color) {
  df <- as_tibble(matrix(hex2HLS(hex_color), ncol=3)) %>%
    mutate_all(as.numeric)
  lab_color <- do.call(HLS, unname(as.list(summarize_all(df, mean))))
  LAB2hex(lab_color)
}

# %% [markdown]
# Investigate food & drink swatches

# %%
food_drink_df <- df %>%
  filter(
    categories %in% c("food", "drink"),
    lightness <= 0.95
  )

# %%
top_df <- food_drink_df %>%
  count(terms, sort = TRUE) %>%
  head(25)

# %%
top_food_drink_df <- food_drink_df %>%
  filter(terms %in% top_df$terms)

# %%
y_max <- 1.1 * max(
  max(-top_food_drink_df$transform_lightness),
  max(top_food_drink_df$transform_lightness)
)
x_min <- floor(min(top_food_drink_df$transform_sat) * 100) / 100
x_max <- ceiling(max(top_food_drink_df$transform_sat) * 100) / 100

# %%
food_drink_colors_df <- top_food_drink_df %>%
  group_by(terms) %>%
  summarize(avg_color = avg_color_hsl(hex)) %>%
  ungroup()

# %%
food_drink_colors <- food_drink_colors_df$avg_color
names(food_drink_colors) <- food_drink_colors_df$terms

# %%
colors <- df$hex
names(colors) <- df$hex

# %%
colors <- c(colors, food_drink_colors)

# %%
terms_df <- food_drink_colors_df %>%
  rowwise() %>%
  mutate(
    lightness = hex2lightness(avg_color),
    sat = hex2sat(avg_color),
    transform_sat = transform(sat, lightness)[1, ],
    transform_lightness = transform(sat, lightness)[2, ]
  ) %>%
  arrange(lightness) %>%
  ungroup()

# %%
n_shades <- food_drink_df %>%
  distinct(brand, product, specific, lightness) %>%
  nrow()

# %%
base_y_max <- max(
  max(-terms_df$transform_lightness),
  max(terms_df$transform_lightness)
)


# %%
base_plot <- terms_df %>%
  ggplot(aes(x = transform_sat, y = transform_lightness, color=terms)) +
  geom_abline(
    intercept = 0,
    slope = 0,
    color = "#efefef",
    size = 0.25,
    linetype = 2
  ) +
  geom_point(
    data = food_drink_df,
    aes(color = hex),
    alpha = 0.1,
    size = 2,
    show.legend = FALSE
  ) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_text_repel(
    aes(label = terms),
    family = "Assistant",
    size = 7,
    point.padding = 0.4,
    min.segment.length = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = enframe(1),
    family = "Assistant",
    x = 0.075,
    y = 0.015,
    size = 5,
    hjust = 0,
    color = "#efefef",
    label = "↑ light",
    fontface = "bold"
  ) +
  geom_text(
    data = enframe(1),
    family = "Assistant",
    x = 0.075,
    y = -0.015,
    size = 5,
    hjust = 0,
    color = "#efefef",
    label = "↓ dark   ← low saturation",
    fontface = "bold"
  ) +
  geom_text(
    data = enframe(1),
    family = "Assistant",
    x = 0.546,
    y = -0.015,
    size = 5,
    hjust = 0,
    color = "#efefef",
    label = "→ high saturation",
    fontface = "bold"
  ) +
  scale_color_manual(values = colors) +
  scale_x_continuous() +
  scale_y_continuous() +
  xlim(x_min, x_max) +
  ylim(-base_y_max, base_y_max) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = "transparent")
  )

# %%
plot_for_term <- function(term) {
  top_food_drink_df %>%
    filter(terms == term) %>%
    ggplot(aes(x = transform_sat, y = transform_lightness)) +
      geom_abline(
        intercept = 0,
        slope = 0,
        color = "#efefef",
        size = 0.25,
        linetype = 2
      ) +
      geom_point(
        aes(color = hex),
        alpha = 1,
        size = 1,
        show.legend = FALSE
      ) +
      geom_mark_hull(
        aes(group = terms, color = terms),
        size = 0.6,
        show.legend = FALSE,
        radius = unit(2, "mm"),
        expand = unit(2, "mm")
      ) +
      scale_color_manual(values = colors) +
      xlim(x_min, x_max) +
      ylim(-y_max, y_max) +
      labs(
        x = "Saturation",
        y = "Lightness",
        title = term
      ) +
      theme_void() +
      theme(
        text = element_text(family = "Assistant", color = "#efefef"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        plot.title = element_text(
          family = "Condiment", size = 22, hjust = 0.5, margin = margin(t = 35)
        )
      )
}

# %%
radius <- 0.4
offset_y <- 0.035
base_size <- 0.65
terms_size <- 0.12

# %%
terms_df = terms_df %>%
  mutate(
    x = radius * cos((row_number() - 1)/ nrow(terms_df) * 2 * pi + 1.025 * pi) + 0.5,
    y = radius * sin((row_number() - 1)/ nrow(terms_df) * 2 * pi + 1.025 * pi) + 0.5
  )

# %%
base_lines_plot <- ggdraw() +
  draw_plot(
    base_plot,
    x = 0.5 - base_size / 2,
    y = 0.5 - base_size / 2 - offset_y,
    width = base_size, height = base_size
  ) +
  draw_line(
    x = c(0.13475, 0.38),
    y = c(0.405, 0.29),
    color = "#784d3b", # Espresso
    size = 0.4
  ) +
  draw_line(
    x = c(0.1226, 0.43),
    y = c(0.5375, 0.7625),
    color = "#e4bb9c", # Vanilla
    size = 0.4
  ) +
  draw_line(
    x = c(0.555, 0.795),
    y = c(0.55, 0.701),
    color = "#c48b60", # Caramel
    size = 0.4
  ) +
  draw_line(
    x = c(0.64825, 0.88),
    y = c(0.5215, 0.4575),
    color = "#c88250", # Chai
    size = 0.4
  ) +
  draw_line(
    x = c(0.64425, 0.77495),
    y = c(0.466, 0.1785),
    color = "#bb7343", # Brown Sugar
    size = 0.4
  )

# %%
terms_plot <- reduce(
  transpose(terms_df),
  .init = base_lines_plot,
  function(p, row) {
    p + draw_plot(
      plot_for_term(row[["terms"]]),
      x = row[["x"]] - terms_size / 2,
      y = row[["y"]] - terms_size / 2 - offset_y,
      width = terms_size, height = terms_size
    )
  }
)

# %%
terms_plot +
  draw_label(
    "Color Shades of Complexion Products named after Food & Drinks",
    y = 0.95, fontfamily = "Condiment", size = 36, color = "#efefef"
  ) +
  draw_label(
    paste(
      "Top 25 Food & Drinks  •  Total",
      format(n_shades, big.mark = ","),
      "Shades"
    ),
    y = 0.21, fontfamily = "Assistant", size = 22, color = "#efefef",
    fontface = "bold"
  ) +
  draw_label(
    "Average colors of surrounding shades clusters\n are shown in the center.",
    y = 0.17, fontfamily = "Assistant", size = 18, color = "#efefef",
    hjust = 0.5, vjust = 0.5, lineheight = 1.1
  ) +
  draw_label(
    "#tidytuesday 14|2021",
    x = 0.08, y = 0.045, fontfamily = "Assistant", size = 14, color = "#efefef",
    fontface = "bold"
  ) +
  draw_label(
    "@spren9er",
    x = 0.935, y = 0.045, fontfamily = "Assistant", size = 14, color = "#efefef",
    fontface = "bold"
  )

# %%
ggsave(
  "images/tidytuesday_202114_makeup.png",
  width = 20, height = 20, dpi = 300, bg = "#333333"
)
