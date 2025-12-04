library(dplyr)
library(tidyr)


load_tree_data <- function(file_name, long = TRUE, data_dir = "data") {
  path <- file.path(data_dir, paste0(file_name, ".csv"))
  data <- read.csv(path, sep = ";")


  data$Tid <- as.POSIXct(data$Tid, format = "%Y-%m-%d %H:%M:%S")


  daily <- data %>%
    mutate(Dato = as.Date(Tid)) %>%
    group_by(Dato) %>%
    summarise(
      Middel  = mean(Temperatur, na.rm = TRUE),
      Laveste = min(Temperatur, na.rm = TRUE),
      Højeste = max(Temperatur, na.rm = TRUE)
    )

  if (!long) {
    return(daily)
  }

  df_long <- pivot_longer(
    daily,
    cols = c("Middel", "Laveste", "Højeste"),
    names_to = "Type",
    values_to = "Value"
  )

  return(df_long)
}

file1 <- "golfpark-eg"

golf_long <- load_tree_data(file1, long = TRUE)

golf_long$Type <- factor(golf_long$Type,
  levels = c("Laveste", "Middel", "Højeste")
)

anova_result <- aov(Value ~ Type, data = golf_long)
summary(anova_result)

TukeyHSD(anova_result)
