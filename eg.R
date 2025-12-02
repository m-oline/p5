source("./load_tree_data.R")
source("./create_line_chart.R")

df_long <- load_tree_data("golfpark-eg")
create_line_chart(
  df_long = df_long,
  file_name = "eg-line",
  title = "Golfpark Eg"
)
