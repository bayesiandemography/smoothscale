

library(dplyr)
library(tidyr)

set.seed(0)

n_area <- 25
mean_area_effect_popn <- 4
sd_area_effect_popn <- 1.5
sd_agesex_popn <- 0.05

sd_area_effect_prev <- 0.5
sd_agesex_prev <- 0.0001
sd_area_agesex_prev <- 0.0001

logit <- function(x) log(x / (1 - x))
invlogit <- function(x) exp(x) / (1 + exp(x))


area <- sprintf("Area %02.0f", seq_len(n_area))

area_effect_popn <- data.frame(area = area,
                               effect = rnorm(n = n_area,
                                              mean = mean_area_effect_popn,
                                              sd = sd_area_effect_popn))
popn <- expand_grid(age = c("5-9", "10-14"),
                    sex = c("Female", "Male"),
                    area = area) %>%
  inner_join(area_effect_popn, by = "area") %>%
  mutate(all_children = rlnorm(n = n(), meanlog = effect, sdlog = sd_agesex_popn),
         all_children = round(all_children))

area_effect_prev <- data.frame(area = area,
                               area_effect = rnorm(n = n_area,
                                                   sd = sd_area_effect_prev))
agesex_effect_prev <- expand_grid(age = c("5-9", "10-14"),
                                  sex = c("Female", "Male")) %>%
    mutate(agesex_effect = logit(c(0.22, 0.24, 0.3, 0.32)))

prev <- cross_join(area_effect_prev, agesex_effect_prev) %>%
    mutate(prev = invlogit(rnorm(n = n(),
                           mean = area_effect + agesex_effect,
                           sd = sd_area_agesex_prev)))

syn_census <- inner_join(popn, prev, by = c("area", "age", "sex")) %>%
    mutate(child_labour = rbinom(n = n(),
                                 size = all_children,
                                 prob = prev)) %>%
    select(area, age, sex, child_labour, all_children)

save(syn_census, file = "../data/syn_census.rda", compress = "bzip2")
