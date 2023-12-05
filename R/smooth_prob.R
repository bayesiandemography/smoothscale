
## HAS_TESTS
#' Smooth Probabilities
#' 
#' Calculate probabilities for multiple groups.
#' Given data on the number of 'trials' in each group,
#' and the number of 'successes' in each group,
#' calculate the probability of success in each group.
#' For instance, given data on the number of respondents
#' in each area, and on the number of employed
#' respondents in area, calculate the probability of being
#' employed in each area. The probabilities are smoothed:
#' the values are shifted towards the overall mean,
#' with values that are based on small sample sizes being
#' shifted the furtherst. 
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
#' @sectiopn `prior_counts`:
#'
#' The argument `prior_counts` controls the degree
#' of smoothing. It only has a noticeable effect
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
#' \deqn{\nu \sim \text{Cauchy}^+(0, M)}
#'
#' where
#'
#' - \eqn{k} is the group,
#' - \eqn{x_k} is the number of successes, which is
#'   specified by argument `x`
#' - \eqn{n_k} is the number of trials, which is
#'   specified by argument `size`
#' - \eqn{\pi_k} is the probability of success, and
#' - \eqn{M} control smoothing, and can be
#'   specified by argument `prior_counts`.
#'
#' `smooth_prob()` returns \eqn{\hat{\pi}_k}, the
#' maximum posterior density estimate of \eqn{\pi_k}.
#' The "direct" (unsmoothed) estimate of the
#' probability of success is \eqn{x_k / n_k}.
#' The smoothed number of counts is \eqn{\hat{\pi}_k x_k}.
#'
#' For details on the model, see the vignette.
#'
#' @param x Number of successes in each group.
#' A numeric vector.
#' @param size Number of trials in each group.
#' A numeric vector.
#' @param prior_cases Parameter controlling
#' smoothing. Default is 100.
#'
#' @returns A numeric vector with the
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
#' rbind(smoothed, unsmoothed)
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
smooth_prob <- function(x, size, prior_cases = 100) {
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




    
