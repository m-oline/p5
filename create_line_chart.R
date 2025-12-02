create_line_chart <- function(df_long, file_name) {
  ggplot(df_long, aes(x = Dato, y = Value, color = Type)) +
    geom_line() +
    geom_point(shape = 21, fill = "transparent", size = 3, stroke = 1) +
    scale_x_datetime(
      breaks = seq(min(df_long$Dato), max(df_long$Dato), by = "2 days"),
      date_labels = "%Y-%m-%d"
    ) +
    scale_y_continuous(
      breaks = seq(-5, max(df_long$Value, na.rm = TRUE), by = 5)
    ) +
    scale_color_manual(
      values = c(
        "Middel" = "green",
        "Laveste" = "blue",
        "Højeste" = "red"
      )
    ) +
    labs(
      y = "Temperatur",
      x = "Dato"
    ) +
    theme_minimal()

  ggsave(paste0("plots/", file_name, ".png"), width = 25, height = 6, dpi = 300)
}
