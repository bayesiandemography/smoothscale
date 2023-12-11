
#' Scale Probabilities
#'
#' Scale probabilities so that they are
#' consistent with pre-specified benchmarks,
#' such as totals obtained from a national survey.
#'
#' @param x Number of successes in each area
#' or population. A numeric vector.
#' @param size Number of trials in each area
#' or population. A numeric vector.
#' @param prob_target Benchmark probability.
#' A number between 0 and 1.
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
#' ## adjust all groups to match benchmark
#' prob_area <- scale_prob(x = census$child_labour,
#'                         size = census$all_children,
#'                         prob_target = prob_national)
#'
#' ## use tidyverse functions to adjust each
#' ## age-sex group to a different total
#' library(dplyr)
#' target <- survey |>
#'   mutate(prob_national = total_child_labour / total_all_children)
#' 
#' census |>
#'   left_join(target, by = c("age", "sex")) |>
#'   group_by(age, sex) |>
#'   mutate(prob_area = scale_prob(x = child_labour,
#'                                 size = all_children,
#'                                 prob_target = prob_national))
#' @export
scale_prob <- function(x, size, prob_target) {
  check_x_size(x = x,
               size = size,
               na_ok = FALSE)
  check_prob_target(prob_target)
  prob_target <- prob_target[[1L]]
  prob_curr <- sum(x) / sum(size)
  err <- prob_target - prob_curr
  prob_direct_vec <- x / size
  scale_up <- err > 0
  if (scale_up)
    scale_vec <- (1 - prob_direct_vec) / (1 - prob_curr)
  else
    scale_vec <- prob_direct_vec / prob_curr
  err_vec <- scale_vec * err
  prob_direct_vec + err_vec
}
