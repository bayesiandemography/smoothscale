
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smoothscale

<!-- badges: start -->
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
  mutate(child_labour_sm = smooth_counts(count = child_labour,
                                         population = all_children))
#> # A tibble: 80 × 6
#>    area    age   sex    child_labour all_children child_labour_sm
#>    <chr>   <chr> <chr>         <int>        <dbl>           <dbl>
#>  1 Area 01 5-9   Female            3            6           2.41 
#>  2 Area 01 5-9   Male              0            5           0.759
#>  3 Area 01 10-14 Female            0            5           0.759
#>  4 Area 01 10-14 Male              0            3           0.564
#>  5 Area 02 5-9   Female            1            3           0.922
#>  6 Area 02 5-9   Male             10           36          10.1  
#>  7 Area 02 10-14 Female            1            3           0.922
#>  8 Area 02 10-14 Male              0            6           0.831
#>  9 Area 03 5-9   Female            4           22           4.48 
#> 10 Area 03 5-9   Male              0            5           0.759
#> # ℹ 70 more rows
```
