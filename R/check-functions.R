
## HAS_TESTS
#' Check 'x' and 'size' arguments
#'
#' Check that 'x' and 'size' both
#' is finite, non-negative numeric, and have
#' same length. If 'na_ok' is FALSE, check
#' that neither have NAs.
#'
#' @param x A numeric vector
#' @param size A numeric vector
#' @param na_ok Logical flag
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_x_size <- function(x, size, na_ok) {
    for (nm in c("x", "size")) {
        value <- get(nm)
        if (!is.numeric(value))
            cli::cli_abort(c("{.arg {nm}} is not numeric.",
                             i = "{.arg {nm}} has class {.cls {class(value)}}."))
        if (any(is.infinite(value)))
            cli::cli_abort("{.arg {nm}} has infinite value.")
        if (any(value[!is.na(value)] < 0))
            cli::cli_abort("{.arg {nm}} has negative value.")
        if (!na_ok)
            if (anyNA(value))
                cli::cli_abort("{.arg {nm}} has NA.")
    }
    if (length(x) != length(size)) 
        cli::cli_abort(c("{.arg x} and {.arg size} have different lengths.",
                         i = "{.arg x} has length {length(x)}.",
                         i = "{.arg size} has length {length(size)}."))
    is_gt <- !is.na(x) & !is.na(size) & (x > size)
    i_gt <- match(TRUE, is_gt, nomatch = 0L)
    if (i_gt > 0L) {
        cli::cli_abort(c("{.arg x} greater than {.arg size}",
                         i = "Element {i_gt} of {.arg x} is {x[[i_gt]]}.",
                         i = "Element {i_gt} of {.arg size} is {size[[i_gt]]}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'prior_cases' argument
#'
#' Check that 'prior_cases' is a non-negative numeric scalar.
#'
#' @param prior_cases Numeric vector of length 1.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prior_cases <- function(prior_cases) {
    if (!is.numeric(prior_cases))
        cli::cli_abort(c("{.arg prior_cases} is not numeric.",
                         i = "{.arg prior_cases} has class {.cls {class(prior_cases)}}."))
    if (!identical(length(prior_cases), 1L))
        cli::cli_abort("{.arg prior_cases} has length {length(prior_cases)}.")
    if (is.infinite(prior_cases))
        cli::cli_abort("{.arg prior_cases} is infinite.")
    if (is.na(prior_cases))
        cli::cli_abort("{.arg prior_cases} is NA.")
    if (prior_cases < 0)
        cli::cli_abort("{.arg prior_cases} is negative.")
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'prob_target' argument
#'
#' Check that 'prob_target' is a numeric vector where,
#' if there is more than one element, the elements
#' are identical. The element(s) must be between
#' 0 and 1.
#'
#' @param prob_target Numeric vector.
#' @param nm Name to be used in error messages.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_prob_target <- function(prob_target, nm) {
    if (!is.numeric(prob_target))
        cli::cli_abort(c("{.arg {nm}} is not numeric.",
                         i = "{.arg {nm}} has class {.cls {class(prob_target)}}."))
    if (anyNA(prob_target))
        cli::cli_abort("{.arg {nm}} has NAs.")
    if (any(prob_target < 0))
        cli::cli_abort("{.arg {nm}} has negative values.")
    if (any(prob_target > 1))
        cli::cli_abort("{.arg {nm}} has values greater than 1.")
    if (identical(length(prob_target), 0L))
        cli::cli_abort("{.arg {nm}} has length 0.")
    if (length(prob_target) > 1L) {
        is_unequal <- prob_target != prob_target[[1L]]
        i_unequal <- match(TRUE, is_unequal, nomatch = 0L)
        if (i_unequal > 0L)
            cli::cli_abort(paste("Element {i_unequal} of {.arg {nm}} ({prob_target[[i_unequal]]})",
                                 "not equal to element 1 ({prob_target[[1L]]})."))
    }
    invisible(TRUE)
}
