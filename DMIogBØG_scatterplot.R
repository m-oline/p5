source("./load_tree_data.R")
source("./load_dmi_data.R")

library(dplyr)
library(ggplot2)

dmi <- load_dmi_data(file_name = "dmi-data.csv") %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "dmi")

bøg <- load_tree_data("golfpark-bøg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

dat <- bind_rows(dmi, bøg)

dat_avg <- dat %>%
  group_by(Dato, group) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

dmi_daily <- dmi %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(dmi = mean(Value, na.rm = TRUE), .groups = "drop")

bog_daily <- bøg %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(`bøg` = mean(Value, na.rm = TRUE), .groups = "drop")

dat_corr <- inner_join(dmi_daily, bog_daily, by = "Dato")

dat_corr_complete <- dat_corr %>%
  filter(!is.na(dmi), !is.na(`bøg`))

if (nrow(dat_corr_complete) > 0) {
  cor_val <- cor(dat_corr_complete$dmi, dat_corr_complete$`bøg`, method = "pearson")
} else {
  cor_val <- NA_real_
}

r_label <- if (is.na(cor_val)) {
  "r: ingen fælles datoer"
} else {
  paste0("r = ", round(cor_val, 2))
}

x_pos <- min(dat_avg$Dato, na.rm = TRUE)
y_pos <- 19

ggplot() +
  geom_point(
    data = dplyr::filter(dat_avg, group == "dmi"),
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
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = r_label,
    hjust = 0, vjust = 1
  ) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(
    x = "Dato",
    y = "Temperatur (°C)",
    title = "Temperaturer fra DMI (grøn) og Bøg (rød) over tid"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
