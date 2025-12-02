library(ggplot2)
library(tidyr)

source("./create_line_chart.R")

data <- read.csv2("./data/dmi-data.csv")
data$Dato <- as.POSIXct(data$Dato, format = "%Y-%m-%d %H:%M:%S")

# Convert data to long format
df_long <- pivot_longer(
  data,
  cols = c("Middel", "Laveste", "Højeste"),
  names_to = "Type",
  values_to = "Value"
)

create_line_chart("dmi-line")

