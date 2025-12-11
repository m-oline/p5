source("./load_tree_data.R")

library(dplyr)
library(ggplot2)

eg <- load_tree_data(file_name = "golfpark-eg") %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "eg")

bøg <- load_tree_data("golfpark-bøg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

dat <- bind_rows(eg, bøg)

dat_avg <- dat %>%
  mutate(Dato = as.Date(Dato)) %>%
  group_by(Dato, group) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

fit_eg <- lm(Value ~ Dato, data = dat_avg %>% filter(group == "eg"))
fit_bøg <- lm(Value ~ Dato, data = dat_avg %>% filter(group == "bøg"))

dat_eg <- dat_avg %>%
  filter(group == "eg") %>%
  mutate(resid = resid(fit_dmi))

dat_bøg <- dat_avg %>%
  filter(group == "bøg") %>%
  mutate(resid = resid(fit_eg))

dat_resid <- bind_rows(dat_eg, dat_bøg)

res_sd <- sd(dat_resid$resid, na.rm = TRUE)

abs_resid <- abs(dat_resid$resid)

n <- sum(!is.na(abs_resid))

share_within_1s <- mean(abs_resid <= res_sd, na.rm = TRUE)
share_between_1_2 <- mean(abs_resid > res_sd & abs_resid <= 2 * res_sd, na.rm = TRUE)
share_outside_2s <- mean(abs_resid > 2 * res_sd, na.rm = TRUE)

cat("Andele af residualer:\n")
cat(sprintf("|r| <= s:      %.1f%%\n", 100 * share_within_1s))
cat(sprintf("s < |r| <= 2s: %.1f%%\n", 100 * share_between_1_2))
cat(sprintf("|r| > 2s:      %.1f%%\n", 100 * share_outside_2s))

shares_label <- paste0(
  "|r| \u2264 s: ",      round(100 * share_within_1s, 1), "%\n",
  "s < |r| \u2264 2s: ", round(100 * share_between_1_2, 1), "%\n",
  "|r| > 2s: ",          round(100 * share_outside_2s, 1), "%"
)

x_min <- min(dat_resid$Dato, na.rm = TRUE)
x_max <- max(dat_resid$Dato, na.rm = TRUE)

res_range <- range(dat_resid$resid, na.rm = TRUE)
y_min <- res_range[1]
y_max <- res_range[2]

y_s_label <- y_max + 0.15 * (y_max - y_min)
y_shares_top <- y_s_label - 1.03 * (y_max - y_min)

break_dates <- as.Date(c("2025-10-01", "2025-10-15", "2025-11-01"))

ggplot(dat_resid, aes(x = Dato, y = resid, color = group)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(
    yintercept = c(-res_sd, res_sd),
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = c(-2 * res_sd, 2 * res_sd),
    linetype = "dotted"
  ) +
  annotate(
    "text",
    x = x_min,
    y = y_s_label,
    hjust = 0, vjust = 1,
    label = paste0("s = ", round(res_sd, 2), " °C")
  ) +
  annotate(
    "text",
    x = x_min,
    y = y_shares_top,
    hjust = 0, vjust = 1,
    label = shares_label
  ) +
  annotate(
    "text",
    x = x_max, y =  2 * res_sd,
    hjust = 1, vjust = -0.2,
    label = "+2s"
  ) +
  annotate(
    "text",
    x = x_max, y =  res_sd,
    hjust = 1, vjust = -0.2,
    label = "+1s"
  ) +
  annotate(
    "text",
    x = x_max, y = -res_sd,
    hjust = 1, vjust = 1.2,
    label = "-1s"
  ) +
  annotate(
    "text",
    x = x_max, y = -2 * res_sd,
    hjust = 1, vjust = 1.2,
    label = "-2s"
  ) +
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
    y = "Residual (°C)",
    title = "Residualplot med s, ±1s, ±2s og andele for Eg og Bøg"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    legend.position = "right"
  )
write.csv(dat_resid, "EGogBØG_dat_resid.csv", row.names = FALSE)