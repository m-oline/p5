source("./load_tree_data.R")
source("./load_dmi_data.R")

library(dplyr)

dmi <- load_dmi_data(file_name = "dmi-hourly.csv") %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "dmi")

eg <- load_tree_data("golfpark-bøg", long = TRUE) %>%
  filter(Type == "Middel") %>%
  select(Dato, Value) %>%
  mutate(group = "bøg")

dat <- bind_rows(dmi, eg) %>%
  mutate(Dag = as.Date(Dato)) # calendar day

dates <- sort(unique(dat$Dag))

result_list <- lapply(dates, function(d) {
  df <- dat[dat$Dag == d & !is.na(dat$Value), ]
  df$group <- droplevels(factor(df$group))

  mod <- aov(Value ~ group, data = df)
  sm <- summary(mod)[[1]]

  F_vals <- sm[, "F value"]
  p_vals <- sm[, "Pr(>F)"]

  # if there is no F or p value (length 0), skip this day
  if (length(F_vals) < 1 || length(p_vals) < 1) {
    # cat("Skipping", d, "- empty F or p\n")
    return(NULL)
  }

  data.frame(
    Dato    = d,
    F.value = F_vals[1],
    p.value = p_vals[1]
  )
})

result_list <- Filter(Negate(is.null), result_list)

if (length(result_list) == 0) {
  stop("No valid ANOVAs could be computed for any day.")
}

result <- do.call(rbind, result_list)

write.table(
  result,
  file = "anova-dmi-bøg.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
