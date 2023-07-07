
library(dplyr)
library(tidyr)

set.seed(0)

load("../data/syn_census.rda")

syn_survey <- syn_census %>%
    group_by(age, sex) %>%
    summarise(child_labour = sum(child_labour),
              all_children = sum(all_children),
              .groups = "drop") %>%
    mutate(total_child_labour = 100 * child_labour,
           total_all_children = 100 * all_children) %>%
    mutate(adj = runif(n = n(), min = 1.15, max = 1.25),
           total_child_labour =  round(adj * total_child_labour)) %>%
    slice(c(3, 4, 1, 2)) %>%
    select(age, sex, total_child_labour, total_all_children)

save(syn_survey, file = "../data/syn_survey.rda", compress = "bzip2")
