library(ggplot2)
library(tidyr)

data$Dato <- as.POSIXct(data$Dato, format = "%Y-%m-%d %H:%M:%S")

# Convert data to long format
df_long <- pivot_longer(
  data,
  cols = c("Middel", "Laveste", "Højeste"),
  names_to = "Type",
  values_to = "Value"
)

ggplot(df_long, aes(x = Dato, y = Value, color = Type)) +
  geom_line() +
  scale_x_datetime(
    breaks = seq(min(df_long$Dato), max(df_long$Dato), by = "2 days"),
    date_labels = "%Y-%m-%d"
  )

ggsave("temperature_plot.png", width = 10, height = 6, dpi = 300)
