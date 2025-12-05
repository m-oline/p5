create_line_chart <- function(df_long,
                              file_name,
                              title,
                              y_label = "Temperatur",
                              type_col = "Type",
                              color_values = c(
                                "Middel"  = "green3",
                                "Laveste" = "blue",
                                "Højeste" = "red"
                              ),
                              legend_labels = c(
                                "Maximum",
                                "Minimum",
                                "Middel"
                              ),
                              show_legend_title = FALSE,
                              date_offset = 0) {
  p <- ggplot(
    df_long,
    aes(
      x = Dato,
      y = Value,
      color = .data[[type_col]]
    )
  ) +
    geom_line() +
    geom_point(shape = 21, fill = "transparent", size = 3, stroke = 1) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      se = FALSE,
      color = "black",
      linewidth = 1
    ) +
    scale_x_datetime(
      breaks = as.POSIXct(seq(
        from = as.Date(min(df_long$Dato)) + date_offset,
        to   = as.Date(max(df_long$Dato)) + date_offset,
        by   = 2
      )),
      date_labels = "%Y-%m-%d"
    ) +
    scale_y_continuous(
      limits = c(-5, 25),
      breaks = seq(-5, 25, by = 5)
    ) +
    {
      if (is.null(color_values)) {
        scale_color_discrete(name = type_col)
      } else {
        scale_color_manual(values = color_values, name = type_col, labels = legend_labels)
      }
    } +
    labs(
      title = title,
      y = y_label,
      x = "Dato"
    ) +
    theme_minimal() +
    theme(
      panel.background   = element_rect(fill = "white", color = NA),
      plot.background    = element_rect(fill = "white", color = NA),
      legend.background  = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),
      plot.title         = element_text(hjust = 0.5),
      legend.title       = if (show_legend_title) element_text() else element_blank()
    )
  
  ggsave(
    paste0("plots/", file_name, ".png"),
    plot   = p,
    width  = 25,
    height = 6,
    dpi    = 300,
    bg     = "white"
  )
  
  invisible(p)
}