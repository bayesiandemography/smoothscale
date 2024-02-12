
## HAS_TESTS
#' Smooth Probabilities
#' 
#' Calculate probabilities for multiple populations.
#' Given data on the number of 'trials' and 'successes'
#' in each population, calculate the
#' probability of success in each population.
#' For instance, given data on the number of respondents,
#' and number of employed respondents, by area,
#' calculate the probability of being
#' employed in each area. The probabilities are smoothed:
#' the values are shifted towards the overall mean,
#' with values that are based on small sample sizes being
#' shifted further than values that are based on large
#' sample sizes. 
#'
#' @section Stratifying:
#'
#' It is often appropriate to stratify a population
#' and smooth separately within each strata.
#' For instance, when estimating probabilities of
#' being employed, it may be appropriate to divide
#' the population into strata defined by age and sex.
#'
#' The easiest way to do stratified smoothing
#' is to use [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frames. See below for an example.
#'
#' @section `prior_counts`:
#'
#' The argument `prior_counts` controls the degree
#' of smoothing. It only has a noticeable effect
#' when the number of areas, and the population per
#' area, is small. Larger values for `prior_counts`
#' produce more smoothing.
#'
#' @section Mathematical details:
#'
#' The smoothing is based on the model
#'
#' \deqn{x_k \sim \text{Binom}(n_k, \pi_k)}
#'
#' \deqn{\pi_k \sim \text{Beta}(\lambda \nu, (1 - \lambda) \nu)}
#'
#' \deqn{\lambda \sim \text{Unif}(0, 1)}
#'
#' \deqn{\nu \sim \text{LogNormal}(\log M, 1)}
#'
#' where
#'
#' - \eqn{k} indexes area or population,
#' - \eqn{x_k} is the number of successes, which is
#'   specified by argument `x`,
#' - \eqn{n_k} is the number of trials, which is
#'   specified by argument `size`,
#' - \eqn{\pi_k} is the probability of success, and
#' - \eqn{M} control smoothing, and can be
#'   specified by argument `prior_counts`.
#' 
#'
#' `smooth_prob()` returns \eqn{\hat{\pi}_k}, the
#' maximum posterior density estimate of \eqn{\pi_k}.
#'
#' The "direct" (unsmoothed) estimate of the
#' probability of success is \eqn{x_k / n_k}.
#'
#'
#' For details on the model, see the vignette
#' [Statistical Models used for Smoothing and Scaling](https://bayesiandemography.github.io/smoothscale/articles/smoothscale-models.html).
#'
#' @param x Number of successes in each population.
#' A numeric vector.
#' @param size Number of trials in each population.
#' A numeric vector.
#' @param prior_cases Parameter controlling
#' smoothing. Default is `10`.
#'
#' @returns A numeric vector with
#' smoothed probabilities.
#'
#' @examples
#' ## use synthetic census data
#' census <- smoothscale::syn_census
#'
#' ## smooth all groups towards the national level
#' smoothed <- smooth_prob(x = census$child_labour,
#'                         size = census$all_children)
#' smoothed
#'
#' ## compare smoothed and unsmoothed ("direct") estimates
#' unsmoothed <- census$child_labour / census$all_children
#' rbind(head(smoothed), head(unsmoothed))
#'
#' ## use tidyverse functions to smooth
#' ## each age-sex group towards a
#' ## different average
#' library(dplyr, warn.conflicts = FALSE)
#' census |>
#'   group_by(age, sex) |>
#'   mutate(smoothed = smooth_prob(x = child_labour,
#'                                 size = all_children))
#' @export
smooth_prob <- function(x, size, prior_cases = 10) {
    check_x_size(x = x,
                 size = size,
                 na_ok = TRUE)
    check_prior_cases(prior_cases)
    alpha_beta <- estimate_alpha_beta(x = x,
                                      size = size,
                                      prior_cases = prior_cases)
    alpha <- alpha_beta[["alpha"]]
    beta <- alpha_beta[["beta"]]
    (x + alpha) / (size + alpha + beta)
}




    
