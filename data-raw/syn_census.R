

library(dplyr)
library(tidyr)

set.seed(0)

n_area <- 20
prior_count <- 5

mean_popn <- data.frame(area = sprintf("Area %02.0f", seq_len(n_area)),
                        mean_popn = seq(from = 5, by = 5, length.out = n_area),
                        area_prev = exp(runif(n = n_area, min = -0.25, max = 0.25)))

mean_prev <- expand_grid(age = c("5-9", "10-14"),
                         sex = c("Female", "Male")) %>%
    mutate(mean_prev = c(0.2, 0.22, 0.3, 0.32))


syn_census <- expand_grid(area = sprintf("Area %02.0f", seq_len(n_area)),
                          age = c("5-9", "10-14"),
                          sex = c("Female", "Male")) %>%
    inner_join(mean_popn, by = "area") %>%
    inner_join(mean_prev, by = c("age", "sex")) %>%
    mutate(all_children = rnbinom(n = n(),
                                  size = 1,
                                  mu = mean_popn) + 2,
           prev = rbeta(n = n(),
                     shape1 = mean_prev * prior_count,
                     shape2 = (1 - mean_prev) * prior_count),
           prev = area_prev * prev,
           prev = pmin(pmax(prev, 0.05), 0.95),
           child_labour = rbinom(n = n(),
                                 size = all_children,
                                 prob = prev)) %>%
    select(area, age, sex, child_labour, all_children)

save(syn_census, file = "../data/syn_census.rda", compress = "bzip2")
