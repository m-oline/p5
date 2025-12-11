library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

api_key <- "c2db9a70-67d5-4234-8998-f3e2789ce92a"

base_url <- "https://dmigw.govcloud.dk/v2/climateData/collections/municipalityValue/items"

start_utc <- "2025-09-21T22:00:00.000Z"
end_utc <- "2025-11-03T22:00:00.000Z"

datetime_range <- paste0(start_utc, "/", end_utc)


get_param_hourly <- function(param_id) {
  res <- GET(
    url = base_url,
    query = list(
      `api-key`      = api_key,
      municipalityId = "0851",
      parameterId    = param_id,
      timeResolution = "hour",
      datetime       = datetime_range,
      limit          = 10000
    )
  )

  stop_for_status(res)

  txt <- content(res, as = "text", encoding = "UTF-8")
  js <- fromJSON(txt)

  props <- js$features$properties

  tibble(
    from_utc = ymd_hms(props$from, tz = "UTC"),
    to_utc   = ymd_hms(props$to, tz = "UTC"),
    value    = props$value
  )
}

df_min <- get_param_hourly("min_temp") %>% rename(min_temp = value)
df_mean <- get_param_hourly("mean_temp") %>% rename(mean_temp = value)
df_max <- get_param_hourly("max_temp_w_date") %>% rename(max_temp = value)

df_all <- df_mean %>%
  rename(datetime_utc = from_utc) %>%
  select(datetime_utc, mean_temp) %>%
  left_join(
    df_min %>% select(from_utc, min_temp),
    by = c("datetime_utc" = "from_utc")
  ) %>%
  left_join(
    df_max %>% select(from_utc, max_temp),
    by = c("datetime_utc" = "from_utc")
  ) %>%
  mutate(
    datetime_local = with_tz(datetime_utc, "Europe/Copenhagen"),
    date_local     = as.Date(datetime_local)
  )


df_all <- df_all %>%
  filter(
    date_local >= as.Date("2025-09-22"),
    date_local <= as.Date("2025-11-03")
  ) %>%
  arrange(datetime_local)

final <- df_all %>%
  mutate(
    Dato = format(datetime_local, "%Y-%m-%d %H:%M:%S")
  ) %>%
  select(
    Dato,
    Laveste = min_temp,
    Middel  = mean_temp,
    Højeste = max_temp
  )

out_file <- "data/dmi-hourly.csv"

write.csv2(final, out_file, row.names = FALSE)

cat(
  "Wrote CSV with", nrow(final), "rows to:\n",
  normalizePath(out_file), "\n"
)
