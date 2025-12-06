source("./load_tree_data.R")

library(dplyr)
library(purrr)
library(broom)

bøg <- load_tree_data("golfpark-bøg")
eg <- load_tree_data("golfpark-eg")

bøg$group <- "bøg"
eg$group <- "eg"

dat <- rbind(bøg, eg)

anova_by_date <- dat %>%
  group_by(Dato) %>%
  do(tidy(aov(Value ~ group, data = .)))

result <- anova_by_date %>%
  filter(term == "group") %>%
  select(Dato, F.value = statistic, p.value)

write.table(
  result,
  file = "anova-bøg-eg.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
tukey_by_date <- dat %>%
  group_by(Dato) %>%
  do(
    aov(Value ~ group, data = .) %>%
      TukeyHSD("group") %>%
      tidy()
  )

write.table(
  tukey_by_date,
  file = "tukey_by_date_results.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
