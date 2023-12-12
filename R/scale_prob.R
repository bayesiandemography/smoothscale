
#' Scale Probabilities
#'
#' Scale probabilities so that they are
#' consistent with pre-specified benchmarks,
#' such as totals obtained from a national survey.
#'
#' @param unscaled Reported probability
#' of having the attribute of interest.
#' A numeric vector with values between 0 and 1.
#' @param target Benchmark probability.
#' A number between 0 and 1.
#' @param wt Weights to use when calculating
#' overall unscaled probability from
#' individual unscaled probabilities. Optional.
#'
#' @returns A numeric vector with scaled
#' probabilities.
#'
#' @seealso [smooth_prob()]
#'
#' @examples
#' set.seed(10)
#' p1 <- runif(n = 10)
#' p2 <- 0.6
#' p1_scaled <- scale_prob(unscaled = p1,
#'                         target = p2)
#' rbind(p1, p1_scaled)
#' mean(p1)
#' mean(p1_scaled)
#' 
#' ## use tidyverse functions to scale separately
#' ## within age-sex groups
#' census <- smoothscale::syn_census
#' survey <- smoothscale::syn_survey
#' library(dplyr)
#' 
#' census |>
#'   left_join(survey, by = c("age", "sex")) |>
#'   mutate(prob_direct = child_labour / all_children) |>
#'   group_by(age, sex) |>
#'   mutate(prob_scaled = scale_prob(unscaled = prob_direct,
#'                                   target = prob_child_labour))
#' @export
scale_prob <- function(unscaled, target, wt = NULL) {
  check_prob(prob = unscaled,
             all_equal = FALSE,
             nm = "unscaled")
  check_prob(prob = target,
             all_equal = TRUE,
             nm = "target")
  target <- target[[1L]]
  has_wt <- !is.null(wt)
  if (has_wt) {
    check_wt(wt = wt, unscaled = unscaled)
    unscaled_mean <- stats::weighted.mean(unscaled, w = wt)
  }
  else
    unscaled_mean <- mean(unscaled)
  err <- target - unscaled_mean
  scale_up <- err > 0
  if (scale_up)
    scale <- (1 - unscaled) / (1 - unscaled_mean)
  else
    scale <- unscaled / unscaled_mean
  unscaled + scale * err
}
