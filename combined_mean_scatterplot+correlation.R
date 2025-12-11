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

bøg <- load_tree_data("golfpark-bøg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

dat <- bind_rows(dmi, eg, bøg)

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

eg_daily <- eg %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(eg = mean(Value, na.rm = TRUE), .groups = "drop")

bog_daily <- bøg %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato) %>%
  summarise(`bøg` = mean(Value, na.rm = TRUE), .groups = "drop")

dat_corr <- dmi_daily %>%
  inner_join(eg_daily, by = "Dato") %>%
  inner_join(bog_daily, by = "Dato")

dat_corr_complete <- dat_corr %>%
  filter(!is.na(dmi), !is.na(eg), !is.na(`bøg`))


if (nrow(dat_corr_complete) > 0) {
  r_dmi_eg <- cor(dat_corr_complete$dmi, dat_corr_complete$eg, method = "pearson")
  r_dmi_bog <- cor(dat_corr_complete$dmi, dat_corr_complete$`bøg`, method = "pearson")
  r_eg_bog <- cor(dat_corr_complete$eg, dat_corr_complete$`bøg`, method = "pearson")
} else {
  r_dmi_eg <- NA_real_
  r_dmi_bog <- NA_real_
  r_eg_bog <- NA_real_
}

r_label <- if (nrow(dat_corr_complete) == 0) {
  "r: ingen fælles datoer"
} else {
  paste0(
    "r(DMI, Eg) = ",  round(r_dmi_eg, 2), "   ",
    "r(DMI, Bøg) = ", round(r_dmi_bog, 2), "   ",
    "r(Eg, Bøg) = ",  round(r_eg_bog, 2)
  )
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
    color = "grey20"
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
      "dmi" = "blue",
      "eg"  = "green4",
      "bøg" = "red"
    ),
    labels = c(
      "dmi" = "DMI = blå",
      "eg"  = "Eg = grøn",
      "bøg" = "Bøg = rød"
    )
  ) +
  labs(
    x = "Dato",
    y = "Temperatur (°C)",
    title = "Korrelation for middeltemperatur"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    legend.position = "right"
  )
