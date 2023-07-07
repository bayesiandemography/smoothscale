
## HAS_TESTS
#' Smooth counts that are subject to sampling variation
#'
#' Take a set of disaggregated counts drawn from
#' disaggregated populations,
#' and smooth towards an overall average,
#' with small counts being smoothed more than large counts.
#'
#' The easiest way to smooth sub-populations towards
#' separate averages is to use
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frames. See the example below.
#'
#' The `prior_cases` argument controls the amount of smoothing.
#' Its effect is only noticeable when the number of cases
#' is very small. Larger values for `prior_cases` produce
#' more smoothing.
#'
#' @param count Number of people sampled who 
#' experienced the outcome of interest.
#' @param size Number of people sampled.
#' @param prior_cases Variable used to control smoothing
#' when the number of cases is small.
#'
#' @returns A numeric vector with smoothed counts.
#'
#' @examples
#' ## use synthetic census data
#' census <- smoothscale::syn_census
#'
#' ## smooth all groups towards the national level
#' smoothed <- smooth_count(count = census$child_labour,
#'                          size = census$all_children)
#'
#' ## a tidyverse-style way of doing the same thing
#' library(dplyr)
#' census %>%
#'   mutate(smoothed = smooth_count(count = child_labour,
#'                                  size = all_children))
#'
#' ## use tidyverse functions to smooth
#' ## each age-sex group towards a
#' ## different average
#' census %>%
#'   group_by(age, sex) %>%
#'   mutate(smoothed = smooth_count(count = child_labour,
#'                                  size = all_children))
#' @export
smooth_count <- function(count, size, prior_cases = 100) {
    check_count_size(count = count,
                           size = size,
                           na_ok = TRUE)
    check_prior_cases(prior_cases)
    alpha_beta <- estimate_alpha_beta(count = count,
                                      size = size,
                                      prior_cases = prior_cases)
    alpha <- alpha_beta[["alpha"]]
    beta <- alpha_beta[["beta"]]
    prevalence_smoothed <- (count + alpha) / (size + alpha + beta)
    prevalence_smoothed * size
}




    
