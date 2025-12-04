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
  filter(term == "group") %>% # keep only the group effect
  select(Dato, F.value = statistic, p.value)

result
