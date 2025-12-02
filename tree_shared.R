library(ggplot2)
library(tidyr)
library(dplyr)

load_tree_data <- function(file_name) {
  data <- read.csv(paste0("data/", file_name, ".csv"), sep = ";")
  str(data)

  data$Tid <- as.POSIXct(data$Tid, format = "%Y-%m-%d %H:%M:%S")

  daily <- data %>%
    mutate(Dato = as.Date(Tid)) %>%
    group_by(Dato) %>%
    summarise(
      Middel  = mean(Temperatur, na.rm = TRUE),
      Laveste = min(Temperatur, na.rm = TRUE),
      Højeste = max(Temperatur, na.rm = TRUE)
    )

  daily$Dato <- as.POSIXct(daily$Dato)

  df_long <- pivot_longer(
    daily,
    cols = c("Middel", "Laveste", "Højeste"),
    names_to = "Type",
    values_to = "Value"
  )
  df_long
}
