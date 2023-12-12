
library(dplyr)
library(tidyr)

set.seed(0)

load("../data/syn_census.rda")

syn_survey <- syn_census %>%
  group_by(age, sex) %>%
  summarise(child_labour = sum(child_labour),
            all_children = sum(all_children),
            .groups = "drop") %>%
  mutate(adj = runif(n = n(), min = 1.15, max = 1.25),
         child_labour =  round(adj * child_labour),
         prob_child_labour = child_labour / all_children,
         prob_child_labour = round(prob_child_labour, 3)) %>%
  select(age, sex, prob_child_labour)  

save(syn_survey, file = "../data/syn_survey.rda", compress = "bzip2")
