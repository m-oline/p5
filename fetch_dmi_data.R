# Fetch hourly min / mean / max temperature from DMI API
# for 2025-09-22 to 2025-11-03 (local Denmark dates)
# and save as a CSV file.

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

## --- SETTINGS ------------------------------------------------------------

# Your API key (you can also read this from an env var if you prefer)
api_key <- "c2db9a70-67d5-4234-8998-f3e2789ce92a"

base_url <- "https://dmigw.govcloud.dk/v2/climateData/collections/municipalityValue/items"

# We want 22 Sep–3 Nov 2025 in Denmark time.
# DMI examples use UTC with a 23:00 day before to 22:00 same day pattern.
# Here we make a single big UTC interval that covers the whole span.
start_utc <- "2025-09-21T22:00:00.000Z" # evening before 22nd in UTC
end_utc <- "2025-11-03T22:00:00.000Z" # evening of 3rd Nov in UTC

datetime_range <- paste0(start_utc, "/", end_utc)

## --- HELPER: fetch one parameterId ---------------------------------------

get_param_hourly <- function(param_id) {
  res <- GET(
    url = base_url,
    query = list(
      `api-key`      = api_key,
      municipalityId = "0851",
      parameterId    = param_id,
      timeResolution = "hour",
      datetime       = datetime_range,
      limit          = 10000 # large enough for the whole period
    )
  )

  stop_for_status(res)

  txt <- content(res, as = "text", encoding = "UTF-8")
  js <- fromJSON(txt)

  # Extract properties (one row per hourly feature)
  props <- js$features$properties

  # Convert to tibble with proper times
  tibble(
    from_utc = ymd_hms(props$from, tz = "UTC"),
    to_utc   = ymd_hms(props$to, tz = "UTC"),
    value    = props$value
  )
}

## --- FETCH MIN / MEAN / MAX ----------------------------------------------

df_min <- get_param_hourly("min_temp") %>% rename(min_temp = value)
df_mean <- get_param_hourly("mean_temp") %>% rename(mean_temp = value)
df_max <- get_param_hourly("max_temp_w_date") %>% rename(max_temp = value)

## --- JOIN ON START TIME ("from") -----------------------------------------

# Use the start time as the key and convert to local Danish time
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

## --- FILTER TO EXACT LOCAL DATES 22 Sep–3 Nov -----------------------------

df_all <- df_all %>%
  filter(
    date_local >= as.Date("2025-09-22"),
    date_local <= as.Date("2025-11-03")
  ) %>%
  arrange(datetime_local)

## --- SELECT FINAL COLUMNS & SAVE CSV -------------------------------------
final <- df_all %>%
  mutate(
    # force a consistent string representation
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
