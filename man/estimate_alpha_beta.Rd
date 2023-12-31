% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_alpha_beta.R
\name{estimate_alpha_beta}
\alias{estimate_alpha_beta}
\title{Estimate Parameters Used in Smoothing}
\usage{
estimate_alpha_beta(x, size, prior_cases)
}
\arguments{
\item{x}{A vector of non-negative values.}

\item{size}{A vector of non-negative values}

\item{prior_cases}{A non-negative scalar.}
}
\value{
A named numeric vector of length 2.
}
\description{
Estimate \code{alpha} and \code{beta} parameters from
\code{x} and \code{size}, by maximising the
posterior density. This is an internal function,
and would not normally be called directly
by end users.
}
\details{
The posterior density is formed by multiplying
a \href{https://en.wikipedia.org/wiki/Beta-binomial_distribution}{beta-binomial}
likelihood by a half-Cauchy prior on the sum
of alpha and beta.
}
\examples{
estimate_alpha_beta(x = syn_census$child_labour,
                    size = syn_census$all_children,
                    prior_cases = 100)
}
\seealso{
\code{estimate_alpha_beta()} is called
by function \code{\link[=smooth_prob]{smooth_prob()}}.
}
\keyword{internal}
