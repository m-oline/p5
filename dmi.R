library(ggplot2)
library(tidyr)

source("./load_dmi_data.R")
source("./create_line_chart.R")

df_long <- load_dmi_data()
create_line_chart(
  df_long = df_long,
  file_name = "dmi-line",
  title = "DMI temperaturer"
)
