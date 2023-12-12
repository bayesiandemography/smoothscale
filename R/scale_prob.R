
#' Scale Probabilities
#'
#' Scale probabilities so that they are
#' consistent with pre-specified benchmarks,
#' such as totals obtained from a national survey.
#'
#' @param prob_report Reported probability
#' of having the attribute of interest.
#' A numeric vector with values between 0 and 1.
#' @param prob_target Benchmark probability.
#' A number between 0 and 1.
#' @param wt Weights to use when calculating
#' overall reported probability from
#' individual reported probabilities. Optional.
#'
#' @returns A numeric vector with scaled
#' probabilities.
#'
#' @seealso [smooth_prob()]
#'
#' @examples
#' ## use synthetic census and survey data
#' census <- smoothscale::syn_census
#' survey <- smoothscale::syn_survey
#'
#' ## calculate national prevalence to use as benchmark
#' prob_national <- sum(survey$total_child_labour) /
#'                    sum(survey$total_all_children)
#'
#' ## calculate direct estimates of area-level probabilities
#' prob_area <- census$child_labour / census$all_children
#'
#' ## adjust all groups to match benchmark
#' prob_scaled <- scale_prob(prob_report = prob_area,
#'                           prob_target = prob_national)
#'
#' ## use tidyverse functions to adjust each
#' ## age-sex group to a different total
#' library(dplyr)
#' target <- survey |>
#'   mutate(prob_national = total_child_labour / total_all_children)
#' 
#' census |>
#'   left_join(target, by = c("age", "sex")) |>
#'   mutate(prob_area = child_labour / all_children) |>
#'   group_by(age, sex) |>
#'   mutate(prob_scaled = scale_prob(prob_report = prob_area,
#'                                   prob_target = prob_national))
#' @export
scale_prob <- function(prob_report, prob_target, wt = NULL) {
  check_prob(prob = prob_report,
             all_equal = FALSE,
             nm = "prob_report")
  check_prob(prob = prob_target,
             all_equal = TRUE,
             nm = "prob_target")
  prob_target <- prob_target[[1L]]
  has_wt <- !is.null(wt)
  if (has_wt) {
    check_wt(wt = wt, prob_report = prob_report)
    prob_report_mean <- stats::weighted.mean(prob_report, w = wt)
  }
  else
    prob_report_mean <- mean(prob_report)
  err <- prob_target - prob_report_mean
  scale_up <- err > 0
  if (scale_up)
    scale <- (1 - prob_report) / (1 - prob_report_mean)
  else
    scale <- prob_report / prob_report_mean
  prob_report + scale * err
}
