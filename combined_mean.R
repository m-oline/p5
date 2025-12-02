library(dplyr)
library(ggplot2)

source("./load_dmi_data.R")
source("./load_tree_data.R")
source("./create_line_chart.R")

df_dmi <- load_dmi_data(long = FALSE)
df_eg <- load_tree_data("golfpark-eg", long = FALSE)
df_bøg <- load_tree_data("golfpark-bøg", long = FALSE)

means_long <- bind_rows(
  df_dmi %>% transmute(Dato, Kilde = "DMI", Value = Middel),
  df_eg %>% transmute(Dato, Kilde = "Eg", Value = Middel),
  df_bøg %>% transmute(Dato, Kilde = "Bøg", Value = Middel)
)

create_line_chart(
  df_long = means_long,
  file_name = "middel-compare",
  title = "Middeltemperatur – DMI vs. Eg vs. Bøg",
  y_label = "Middeltemperatur",
  type_col = "Kilde",
  color_values = c(
    "DMI" = "black",
    "Eg"  = "darkgreen",
    "Bøg" = "brown"
  ),
  show_legend_title = FALSE
)
