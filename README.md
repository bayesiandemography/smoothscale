
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
  mutate(smoothed = smooth_prob(x = child_labour,
                                size = all_children),
     scaled = scale_prob(unscaled = smoothed,
                         target = prob_child_labour))
#> # A tibble: 100 × 8
#> # Groups:   age, sex [4]
#>    area  age   sex   child_labour all_children prob_child_labour smoothed scaled
#>    <chr> <chr> <chr>        <int>        <dbl>             <dbl>    <dbl>  <dbl>
#>  1 Area… 5-9   Fema…          134          372             0.297   0.351   0.414
#>  2 Area… 5-9   Fema…           14           35             0.297   0.325   0.390
#>  3 Area… 5-9   Fema…           92          388             0.297   0.236   0.310
#>  4 Area… 5-9   Fema…           46          345             0.297   0.140   0.223
#>  5 Area… 5-9   Fema…           25          102             0.297   0.241   0.314
#>  6 Area… 5-9   Fema…            2            5             0.297   0.252   0.324
#>  7 Area… 5-9   Fema…            4           13             0.297   0.252   0.324
#>  8 Area… 5-9   Fema…           10           34             0.297   0.264   0.335
#>  9 Area… 5-9   Fema…            2           52             0.297   0.0995  0.186
#> 10 Area… 5-9   Fema…          578         2087             0.297   0.276   0.346
#> # ℹ 90 more rows
```
