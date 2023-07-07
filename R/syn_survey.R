
#' A synthetic survey dataset
#'
#' A synthetic (ie made-up) dataset, illustrating the
#' sort of ta bulations than can be produced from a survey
#' file. The synthetic data measure child labour
#' at the national level
#'
#' @format
#' A [tibble][tibble::tibble()] with 4 rows and the following columns:
#'
#' - `age`: `"5-9" or `"10-14"`
#' - `sex`: `"Female"` or `"Male"`
#' - `total_child_labour`: Estimated number of children involved in
#'   child labour in the whole country
#' - `total_all_children`: Estimated number of children, including
#'   those not involved in child labour, in the whole country
"syn_survey"
