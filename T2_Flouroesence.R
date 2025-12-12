library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)

t2_path <- file.choose() # choose golfpark-bøg.csv
fluo_path <- file.choose() # choose gennemsnit-flouroesence.csv

t2_raw <- read.csv(t2_path, sep = ";", check.names = FALSE, stringsAsFactors = FALSE)

t2_day <- t2_raw %>%
  transmute(
    Tid = as.POSIXct(Tid, format = "%Y-%m-%d %H:%M:%S"),
    Dato = as.Date(Tid),
    T1 = as.numeric(Temperatur)
  ) %>%
  group_by(Dato) %>%
  summarise(T2 = mean(T2, na.rm = TRUE), .groups = "drop")


fluo_wide <- read.csv2(fluo_path, check.names = FALSE, stringsAsFactors = FALSE)


if (!("Træ" %in% names(fluo_wide)) && !("Tree" %in% names(fluo_wide))) {
  fluo_wide <- rownames_to_column(fluo_wide, var = "Tree")
} else {
  names(fluo_wide)[1] <- "Tree"
}

fluo_long <- fluo_wide %>%
  pivot_longer(
    cols = -Tree,
    names_to = "Dato",
    values_to = "Fluo"
  ) %>%
  mutate(
    Dato = as.Date(as.character(Dato), format = "%m/%d/%y"),
    Fluo = as.numeric(gsub(",", ".", as.character(Fluo)))
  ) %>%
  filter(!is.na(Dato))

dat <- fluo_long %>%
  left_join(t1_day, by = "Dato")

View(dat)

dat_scatter <- dat %>%
  filter(!is.na(T2), !is.na(Fluo))

ct_all <- cor.test(dat_scatter$T2, dat_scatter$Fluo, method = "pearson")

corr_label <- sprintf(
  "n = %d\nr = %.3f\np = %.3g",
  sum(complete.cases(dat_scatter$T2, dat_scatter$Fluo)),
  unname(ct_all$estimate),
  ct_all$p.value
)

cat("\nOVERALL correlation (all trees pooled):\n")
cat(sprintf("n = %d\n", sum(complete.cases(dat_scatter$T2, dat_scatter$Fluo))))
cat(sprintf("r = %.4f\n", unname(ct_all$estimate)))
cat(sprintf("p = %.4g\n\n", ct_all$p.value))

corr_by_tree <- dat_scatter %>%
  group_by(Tree) %>%
  summarise(
    n = sum(complete.cases(T2, Fluo)),
    r = ifelse(n >= 3, cor(T2, Fluo, use = "complete.obs"), NA_real_),
    p = ifelse(n >= 3, cor.test(T2, Fluo)$p.value, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

View(corr_by_tree)
print(corr_by_tree)

p <- ggplot(dat_scatter, aes(x = T2, y = Fluo, color = Tree)) +
  geom_point(alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.1, label = corr_label) +
  labs(
    x = "T2 (Temperatur, dagligt middel)",
    y = "Fluorescens",
    title = "Fluorescens vs T2 (alle datapunkter)",
    color = "Tree"
  ) +
  theme_minimal()

print(p)


write.csv(dat, "merged_fluorescens_plus_T2.csv", row.names = FALSE)
write.csv(dat_scatter, "scatterdata_fluorescens_vs_T2.csv", row.names = FALSE)
write.csv(corr_by_tree, "corr_by_tree_T1.csv", row.names = FALSE)
ggsave("scatter_fluorescens_vs_T2.png", plot = p, width = 9, height = 6, dpi = 300)
