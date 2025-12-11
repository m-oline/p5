source("./load_tree_data.R")

library(dplyr)
library(ggplot2)

eg <- load_tree_data("golfpark-eg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "eg")


bøg <- load_tree_data("golfpark-bøg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

dat <- bind_rows(eg, bøg)

dat_avg <- dat %>%
  group_by(Dato, group) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot() +
  geom_point(
    data = dplyr::filter(dat_avg, group == "eg"),
    aes(x = Dato, y = Value),
    color = "green"
  ) +
  geom_point(
    data = dplyr::filter(dat_avg, group == "bøg"),
    aes(x = Dato, y = Value),
    color = "red"
  ) +
  geom_smooth(
    data = dat_avg,
    aes(x = Dato, y = Value),
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  labs(
    x = "Dato",
    y = "Temperatur (°C)",
    title = "Temperaturer fra Eg (grøn) og Bøg (rød) over tid"
  ) +
  theme_minimal()
