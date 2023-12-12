
#' Scale Probabilities
#'
#' Scale probabilities so that they are
#' consistent with pre-specified benchmarks,
#' such as totals obtained from a national survey.
#'
#' @param prob_unscaled Reported probability
#' of having the attribute of interest.
#' A numeric vector with values between 0 and 1.
#' @param prob_target Benchmark probability.
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
#' p1_scaled <- scale_prob(prob_unscaled = p1,
#'                         prob_target = p2)
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
#'   mutate(prob_scaled = scale_prob(prob_unscaled = prob_direct,
#'                                   prob_target = prob_child_labour))
#' @export
scale_prob <- function(prob_unscaled, prob_target, wt = NULL) {
  check_prob(prob = prob_unscaled,
             all_equal = FALSE,
             nm = "prob_unscaled")
  check_prob(prob = prob_target,
             all_equal = TRUE,
             nm = "prob_target")
  prob_target <- prob_target[[1L]]
  has_wt <- !is.null(wt)
  if (has_wt) {
    check_wt(wt = wt, prob_unscaled = prob_unscaled)
    prob_unscaled_mean <- stats::weighted.mean(prob_unscaled, w = wt)
  }
  else
    prob_unscaled_mean <- mean(prob_unscaled)
  err <- prob_target - prob_unscaled_mean
  scale_up <- err > 0
  if (scale_up)
    scale <- (1 - prob_unscaled) / (1 - prob_unscaled_mean)
  else
    scale <- prob_unscaled / prob_unscaled_mean
  prob_unscaled + scale * err
}
