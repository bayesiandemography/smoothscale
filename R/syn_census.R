
#' A synthetic census dataset
#'
#' A synthetic (ie made-up) dataset, illustrating the
#' sort of data that a census or survey. The synthetic data
#' measure child labour.
#'
#' @format
#' A [tibble][tibble::tibble()] with 80 rows and the following columns:
#'
#' - `area`: Geographical area, numbered from 1 to 20
#' - `age`: `"5-9" or `"10-14"`
#' - `sex`: `"Female"` or `"Male"`
#' - `child_labour`: Number of children involved in
#'   child labour
#' - `all_children`: Total number of children, including
#'   those not involved in child labour
"syn_census"
