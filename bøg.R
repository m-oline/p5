source("./load_tree_data.R")
source("./create_line_chart.R")

df_long <- load_tree_data("golfpark-bøg")
create_line_chart(df_long, "bøg-line")
