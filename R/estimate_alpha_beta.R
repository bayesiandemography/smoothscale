
#' Estimate parameters used in smoothing
#'
#' Estimate `alpha` and `beta` parameters from
#' `count` and `size`, by maximising the
#' posterior density. This is an internal function,
#' and would not normally be called directly
#' by end users.
#'
#' The posterior density is formed by multiplying
#' a [beta-binomial](https://en.wikipedia.org/wiki/Beta-binomial_distribution)
#' likelihood by a log-normal prior on the sum
#' of alpha and beta.
#'
#' @param count A vector of non-negative values.
#' @param size A vector of non-negative values
#' @param prior_cases A non-negative scalar.
#'
#' @returns A named numeric vector of length 2.
#'
#' @seealso `estimate_alpha_beta()` is called
#' by function [smooth_count()].
#'
#' @examples
#' estimate_alpha_beta(count = syn_census$child_labour,
#'                     size = syn_census$all_children,
#'                     prior_cases = 10)
#' @keywords internal
#' @export
estimate_alpha_beta <- function(count, size, prior_cases) {
    keep <- !is.na(count) & !is.na(size) & (size > 0)
    count <- count[keep]
    size <- size[keep]
    neg_log_post <- function(x) {
        alpha <- exp(x[[1L]])
        beta <- exp(x[[2L]])
        n <- length(count)
        log_lik <- (sum(lbeta(count + alpha, size - count + beta))
            - n * lbeta(alpha, beta))
        log_prior <- stats::dlnorm(alpha + beta,
                                   meanlog = log(prior_cases),
                                   sdlog = 1,
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
