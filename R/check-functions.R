
## HAS_TESTS
#' Check 'count' and 'size' arguments
#'
#' Check that 'count' and 'size' both
#' is finite, non-negative numeric, and have
#' same length. If 'na_ok' is FALSE, check
#' that neither have NAs.
#'
#' @param count A numeric vector
#' @param size A numeric vector
#' @param na_ok Logical flag
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_count_size <- function(count, size, na_ok) {
    for (nm in c("count", "size")) {
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
    if (length(count) != length(size)) 
        cli::cli_abort(c("{.arg count} and {.arg size} have different lengths.",
                         i = "{.arg count} has length {length(count)}.",
                         i = "{.arg size} has length {length(size)}."))
    is_gt <- !is.na(count) & !is.na(size) & (count > size)
    i_gt <- match(TRUE, is_gt, nomatch = 0L)
    if (i_gt > 0L) {
        cli::cli_abort(c("{.arg count} greater than {.arg size}",
                         i = "Element {i_gt} of {.arg count} is {count[[i_gt]]}.",
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
#' Check 'total' argument
#'
#' Check that 'total' is a non-negative numeric vector,
#' with identical elements.
#'
#' @param total Numeric vector.
#' @param nm Name to be used in error messages.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_total <- function(total, nm) {
    if (!is.numeric(total))
        cli::cli_abort(c("{.arg {nm}} is not numeric.",
                         i = "{.arg {nm}} has class {.cls {class(total)}}."))
    if (any(is.infinite(total)))
        cli::cli_abort("{.arg {nm}} has infinite values.")
    if (anyNA(total))
        cli::cli_abort("{.arg {nm}} has NAs.")
    if (any(total < 0))
        cli::cli_abort("{.arg {nm}} has negative values.")
    if (identical(length(total), 0L))
        cli::cli_abort("{.arg {nm}} has length 0.")
    if (length(total) > 1L) {
        is_unequal <- total != total[[1L]]
        i_unequal <- match(TRUE, is_unequal, nomatch = 0L)
        if (i_unequal > 0L)
            cli::cli_abort(paste("Element {i_unequal} of {.arg {nm}} ({total[[i_unequal]]})",
                                 "not equal to element 1 ({total[[1L]]})."))
    }
    invisible(TRUE)
}
