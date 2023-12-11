
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smoothscale

<!-- badges: start -->

[![R-CMD-check](https://github.com/bayesiandemography/smoothscale/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/smoothscale/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/smoothscale/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/smoothscale?branch=main)
<!-- badges: end -->

Simple small area estimation methods.

## Installation

``` r
devtools::install_github("bayesiandemography/smoothscale")
```

## Example

``` r
library(smoothscale)
library(dplyr, warn.conflicts = FALSE)
syn_census %>%
  inner_join(syn_survey, by = c("age", "sex")) %>%
  group_by(age, sex) %>%
  mutate(child_labour_sm = smooth_prob(x = child_labour,
                                       size = all_children))
#> # A tibble: 200 × 8
#> # Groups:   age, sex [4]
#>    area    age   sex    child_labour all_children total_child_labour
#>    <chr>   <chr> <chr>         <int>        <dbl>              <dbl>
#>  1 Area 01 5-9   Female           42          368             222365
#>  2 Area 02 5-9   Female           10           33             222365
#>  3 Area 03 5-9   Female          112          453             222365
#>  4 Area 04 5-9   Female          151          354             222365
#>  5 Area 05 5-9   Female           23          101             222365
#>  6 Area 06 5-9   Female            3            5             222365
#>  7 Area 07 5-9   Female            3           14             222365
#>  8 Area 08 5-9   Female            6           35             222365
#>  9 Area 09 5-9   Female           14           48             222365
#> 10 Area 10 5-9   Female          422         1889             222365
#> # ℹ 190 more rows
#> # ℹ 2 more variables: total_all_children <dbl>, child_labour_sm <dbl>
```
