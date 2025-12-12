library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)

# ---------- helpers ----------
to_num <- function(x) {
  x |>
    as.character() |>
    gsub("\\s+", "", x = _) |>
    gsub("[^0-9,.-]", "", x = _) |>
    gsub(",", ".", x = _) |>
    gsub("\\.$", "", x = _) |>
    as.numeric()
}

parse_dato <- function(x) {
  as.Date(parse_date_time(as.character(x),
                          orders = c("Y-m-d","d-m-Y","d/m/Y","m/d/y","m/d/Y","Y/m/d")))
}

# ---------- choose files ----------
dmi_path  <- file.choose()  # DMI_middel.csv
fluo_path <- file.choose()  # gennemsnit-flouroesence.csv

# ---------- DMI (Middel) : one value per date ----------
dmi_day <- read.csv2(dmi_path, check.names = FALSE, stringsAsFactors = FALSE) %>%
  transmute(
    Dato = parse_dato(Dato),
    DMI  = to_num(Middel)
  ) %>%
  filter(!is.na(Dato)) %>%
  group_by(Dato) %>%
  summarise(DMI = mean(DMI, na.rm = TRUE), .groups = "drop")

# ---------- Fluorescens: KEEP ALL DATA POINTS ----------
fluo_raw <- read.csv2(fluo_path, check.names = FALSE, stringsAsFactors = FALSE)

# Detect if file already has a date column
date_col <- names(fluo_raw)[grepl("^dato$|^date$", names(fluo_raw), ignore.case = TRUE)]

if (length(date_col) >= 1) {
  # Case A: already long-ish (has Dato column) -> pivot other columns to keep ALL points
  date_col <- date_col[1]
  
  fluo_long <- fluo_raw %>%
    mutate(Dato = parse_dato(.data[[date_col]])) %>%
    select(-all_of(date_col)) %>%
    mutate(row_id = row_number()) %>%   # keep uniqueness if many columns
    pivot_longer(
      cols = -c(Dato, row_id),
      names_to = "Series",
      values_to = "Fluo"
    ) %>%
    mutate(Fluo = to_num(Fluo)) %>%
    filter(!is.na(Dato))
  
} else {
  # Case B: wide (dates are columns, Tree/ID in first column) -> pivot to keep ALL points
  if (!("Træ" %in% names(fluo_raw)) && !("Tree" %in% names(fluo_raw)) && !("ID" %in% names(fluo_raw))) {
    fluo_raw <- rownames_to_column(fluo_raw, var = "Tree")
  } else {
    names(fluo_raw)[1] <- "Tree"
  }
  
  fluo_long <- fluo_raw %>%
    pivot_longer(
      cols = -Tree,
      names_to = "Dato",
      values_to = "Fluo"
    ) %>%
    mutate(
      Dato = parse_dato(Dato),
      Fluo = to_num(Fluo)
    ) %>%
    filter(!is.na(Dato))
}

# ---------- merge (DMI is repeated across all points on same date) ----------
dat <- fluo_long %>%
  left_join(dmi_day, by = "Dato")

View(dat)

dat_scatter <- dat %>% filter(!is.na(DMI), !is.na(Fluo))

# ---------- overall correlation ----------
ct_all <- cor.test(dat_scatter$DMI, dat_scatter$Fluo, method = "pearson")

corr_label <- sprintf(
  "n = %d\nr = %.3f\np = %.3g",
  sum(complete.cases(dat_scatter$DMI, dat_scatter$Fluo)),
  unname(ct_all$estimate),
  ct_all$p.value
)

cat("\nOVERALL correlation (all datapoints):\n")
cat(sprintf("n = %d\n", sum(complete.cases(dat_scatter$DMI, dat_scatter$Fluo))))
cat(sprintf("r = %.4f\n", unname(ct_all$estimate)))
cat(sprintf("p = %.4g\n\n", ct_all$p.value))

# ---------- correlation by group if Tree exists ----------
if ("Tree" %in% names(dat_scatter)) {
  corr_by_tree <- dat_scatter %>%
    group_by(Tree) %>%
    summarise(
      n = sum(complete.cases(DMI, Fluo)),
      r = ifelse(n >= 3, cor(DMI, Fluo, use = "complete.obs"), NA_real_),
      p = ifelse(n >= 3, cor.test(DMI, Fluo)$p.value, NA_real_),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  View(corr_by_tree)
  print(corr_by_tree)
}

# ---------- plot ----------
p <- ggplot(dat_scatter, aes(x = DMI, y = Fluo)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, label = corr_label) +
  labs(
    x = "DMI (Middel)",
    y = "Gennemsnit fluorescens (alle datapunkter)",
    title = "Fluorescens vs DMI (alle datapunkter)"
  ) +
  theme_minimal()

# color by Tree if present
if ("Tree" %in% names(dat_scatter)) {
  p <- p + aes(color = Tree) + labs(color = "Tree")
}

print(p)

# ---------- outputs ----------
write.csv(dat, "merged_gennemsnit_fluo_plus_dmi_ALLPOINTS.csv", row.names = FALSE)
write.csv(dat_scatter, "scatterdata_gennemsnit_fluo_vs_dmi_ALLPOINTS.csv", row.names = FALSE)
if (exists("corr_by_tree")) write.csv(corr_by_tree, "corr_by_tree_ALLPOINTS.csv", row.names = FALSE)
ggsave("scatter_gennemsnit_fluo_vs_dmi_ALLPOINTS.png", plot = p, width = 9, height = 6, dpi = 300)