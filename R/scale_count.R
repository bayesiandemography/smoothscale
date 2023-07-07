
#' Scale counts to match totals
#'
#' Scale counts so that they add up to
#' pre-specified totals, such as totals
#' obtained from a national survey.
#'
#' @param count Number of people sampled who 
#' experienced the outcome of interest.
#' @param size Number of people sampled.
#' @param count_total Target number that `count`
#' should add up to.
#' @param size_total Target number that `size`
#' should add up to.
#'
#' @returns A numeric vector with scaled counts.
#'
#' @seealso [smooth_count()]
#'
#' @examples
#' ## use synthetic census and survey data
#' census <- smoothscale::syn_census
#' survey <- smoothscale::syn_survey
#'
#' ## adjust all groups to match combined total
#' national_child_labour <- 1104194
#' national_all_children <- 3035800
#' child_labour_sc <- scale_count(count = census$child_labour,
#'                                size = census$all_children,
#'                                count_total = national_child_labour,
#'                                size_total = national_all_children)
#'
#' ## a tidyverse-style way of doing the same thing
#' library(dplyr)
#' census %>%
#'   mutate(child_labour_sc = scale_count(count = child_labour,
#'                                        size = all_children,
#'                                        count_total = national_child_labour,
#'                                        size_total = national_all_children))
#'
#' ## use tidyverse functions to adjust each
#' ## age-sex group to a different total
#' scaled <- census %>%
#'   left_join(survey, by = c("age", "sex")) %>%
#'   group_by(age, sex) %>%
#'   mutate(child_labour_sc = scale_count(count = child_labour,
#'                                        size = all_children,
#'                                        count_total = national_child_labour,
#'                                        size_total = national_all_children))
#' scaled
#' scaled %>%
#'   count(age, sex, wt = child_labour_sc)
#' @export
scale_count <- function(count, size, count_total, size_total) {
    check_count_size(count = count,
                     size = size,
                     na_ok = FALSE)
    check_total(count_total, nm = "count_total")
    check_total(size_total, nm = "size_total")
    count_total <- count_total[[1L]]
    size_total <- size_total[[1L]]
    if (count_total > size_total)
        cli::cli_abort(c("{.arg count_total} is larger than {.arg size_total}.",
                         i = "{.arg count_total} is {count_total}.",
                         i = "{.arg size_total} is {size_total}."))
    multiplier_overall <- size_total / sum(size)
    count <- multiplier_overall * count
    size <- multiplier_overall * size
    multiplier_specific <- (count_total - sum(count)) / (size_total - sum(count))
    count + multiplier_specific * (size - count)
}
