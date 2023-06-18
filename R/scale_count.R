
#' Scale counts to match totals
#'
#' Scale counts so that they add up to
#' pre-specified totals, such as totals
#' obtained from a national survey.
#'
#' @param count Number of people sampled who 
#' experienced the outcome of interest.
#' @param population Number of people sampled.
#' @param total Target number that `count`
#' should add up to.
#'
#' @returns A numeric vector with scaled counts.
#'
#' @seealso [smooth_count()]
#'
#' @examples
#' ## use synthetic census data
#' census <- smoothscale::syn_census
#'
#' ## adjust all groups to match combined total
#' national_total <- 1450
#' child_labour_sc <- scale_count(count = census$child_labour,
#'                                population = census$all_children,
#'                                total = national_total)
#'
#' ## a tidyverse-style way of doing the same thing
#' library(dplyr)
#' census %>%
#'   mutate(child_labour_sc = scale_count(count = child_labour,
#'                                        population = all_children,
#'                                        total = national_total))
#'
#' ## use tidyverse functions to adjust each
#' ## age-sex group to a different total
#' national_total <- tribble(
#'   ~age,  ~sex,     ~national_total,
#'   "5-9", "Female", 200,
#'   "5-9", "Male",   260,
#' "10-14", "Female", 600,
#' "10-14", "Male",   420)
#' scaled <- census %>%
#'   left_join(national_total, by = c("age", "sex")) %>%
#'   group_by(age, sex) %>%
#'   mutate(child_labour_sc = scale_count(count = child_labour,
#'                                        population = all_children,
#'                                        total = national_total))
#' scaled
#' scaled %>%
#'   count(age, sex, wt = child_labour_sc)
#' @export
scale_count <- function(count, population, total) {
    check_count_population(count = count,
                           population = population,
                           na_ok = FALSE)
    check_total(total)
    total <- unique(total)
    multiplier <- (total - sum(count)) / (sum(population) - sum(count))
    count + multiplier * (population - count)
}
