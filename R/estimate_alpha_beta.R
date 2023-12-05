
#' Estimate Parameters Used in Smoothing
#'
#' Estimate `alpha` and `beta` parameters from
#' `x` and `size`, by maximising the
#' posterior density. This is an internal function,
#' and would not normally be called directly
#' by end users.
#'
#' The posterior density is formed by multiplying
#' a [beta-binomial](https://en.wikipedia.org/wiki/Beta-binomial_distribution)
#' likelihood by a half-Cauchy prior on the sum
#' of alpha and beta.
#'
#' @param x A vector of non-negative values.
#' @param size A vector of non-negative values
#' @param prior_cases A non-negative scalar.
#'
#' @returns A named numeric vector of length 2.
#'
#' @seealso `estimate_alpha_beta()` is called
#' by function [smooth_prob()].
#'
#' @examples
#' estimate_alpha_beta(x = syn_census$child_labour,
#'                     size = syn_census$all_children,
#'                     prior_cases = 100)
#' @keywords internal
#' @export
estimate_alpha_beta <- function(x, size, prior_cases) {
    keep <- !is.na(x) & !is.na(size) & (size > 0)
    x <- x[keep]
    size <- size[keep]
    n <- length(x)
    neg_log_post <- function(y) {
        alpha <- exp(y[[1L]])
        beta <- exp(y[[2L]])
        log_lik <- (sum(lbeta(x + alpha, size - x + beta))
            - n * lbeta(alpha, beta))
        log_prior <- stats::dcauchy(alpha + beta,
                                    scale = prior_cases,
                                    log = TRUE)
        -1 * (log_lik + log_prior)
    }
    val <- stats::optim(par = c(1, 1), fn = neg_log_post)
    convergence <- val$convergence
    if (convergence != 0L)
        cli::cli_abort(c("Optimiser did not converge.",
                         i = "Convergence code: {convergence}."))
    c(alpha = exp(val$par[[1L]]),
      beta = exp(val$par[[2L]]))
}
