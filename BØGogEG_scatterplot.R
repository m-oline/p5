source("./load_tree_data.R")

library(dplyr)
library(ggplot2)

bøg <- load_tree_data(file_name = "golfpark-bøg") %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

eg <- load_tree_data("golfpark-eg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "eg")

dat <- bind_rows(bøg, eg)

dat_avg <- dat %>%
  group_by(Dato, group) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

bog_daily <- bøg %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(bøg = mean(Value, na.rm = TRUE), .groups = "drop")

eg_daily <- eg %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(`eg` = mean(Value, na.rm = TRUE), .groups = "drop")

dat_corr <- inner_join(bog_daily, eg_daily, by = "Dato")

dat_corr_complete <- dat_corr %>%
  filter(!is.na(bøg), !is.na(`eg`))

if (nrow(dat_corr_complete) > 0) {
  cor_val <- cor(dat_corr_complete$bøg, dat_corr_complete$`eg`, method = "pearson")
} else {
  cor_val <- NA_real_
}

r_label <- if (is.na(cor_val)) {
  "r: ingen fælles datoer"
} else {
  paste0("r = ", round(cor_val, 2))
}

dat_avg$Dato <- as.Date(dat_avg$Dato)

x_pos <- min(dat_avg$Dato, na.rm = TRUE)
y_pos <- 19

break_dates <- as.Date(c("2025-10-01", "2025-10-15", "2025-11-01"))


ggplot(dat_avg, aes(x = Dato, y = Value)) +
  geom_point(aes(color = group)) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = r_label,
    hjust = 0,
    vjust = 1
  ) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_date(
    breaks = break_dates,
    date_labels = "%d-%m-%Y"
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "bøg" = "green",
      "eg"  = "red"
    ),
    labels = c(
      "bøg" = "Bøg = grøn",
      "eg"  = "Eg = rød"
    )
  ) +
  labs(
    x = "Dato",
    y = "Temperatur (°C)",
    title = "Korrelation for T2 og T1 over tid"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    legend.position = "right"
  )
