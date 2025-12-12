load_flouroesence_data <- function(long = TRUE,
                                   file_name = "gennemsnit-flouroesence.csv",
                                   sheet = 1) {
  path <- file.path("./data", file_name)
  ext  <- tolower(tools::file_ext(file_name))
  
  if (ext %in% c("xlsx", "xls")) {
    data <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  } else {
    data <- read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE)
  }

  names(data)[1] <- "Tree"
  
  if (!long) return(data)
  
  df_long <- tidyr::pivot_longer(
    data,
    cols = -Tree,
    names_to = "Dato",
    values_to = "Value",
    values_transform = list(Value = as.numeric)
  )
  df_long$Dato <- lubridate::as_datetime(
    lubridate::parse_date_time(
      as.character(df_long$Dato),
      orders = c("Y-m-d H:M:S", "Y-m-d", "m/d/y")
    )
  )
  
  df_long$Type <- "Middel"
  dplyr::select(df_long, Dato, Type, Value, Tree)
}