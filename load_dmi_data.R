load_dmi_data <- function(long = TRUE, file_name = "dmi-data.csv") {
  path <- paste0("./data/", file_name)
  data <- read.csv2(path)

  data$Dato <- as.POSIXct(data$Dato, format = "%Y-%m-%d %H:%M:%S")

  if (!long) {
    return(data)
  }

  df_long <- pivot_longer(
    data,
    cols = c("Middel", "Laveste", "Højeste"),
    names_to = "Type",
    values_to = "Value"
  )
  df_long
}
