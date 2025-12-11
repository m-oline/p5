source("./load_tree_data.R")
source("./load_dmi_data.R")

library(dplyr)
library(ggplot2)

dmi <- load_dmi_data(file_name = "dmi-data.csv") %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "dmi")


eg <- load_tree_data("golfpark-eg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "eg")

dat <- bind_rows(dmi, eg)

dat_avg <- dat %>%
  group_by(Dato, group) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot() +
  geom_point(
    data = dplyr::filter(dat_avg, group == "dmi"),
    aes(x = Dato, y = Value),
    color = "green"
  ) +
  geom_point(
    data = dplyr::filter(dat_avg, group == "eg"),
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
    title = "Temperaturer fra DMI (grøn) og Eg (rød) over tid"
  ) +
  theme_minimal()
