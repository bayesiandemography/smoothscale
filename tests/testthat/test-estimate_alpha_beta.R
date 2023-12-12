
test_that("'estimate_alpha_beta' works with valid inputs", {
  set.seed(0)
  n <- 500
  for (prior_cases in c(10, 100, 1000)) {
    for (i in seq_len(20)) {
      prob <- runif(1)
      size <- rpois(n = n, lambda = 20)
      prior_cases <- 10
      x <- rbinom(n = n, size = size, prob = prob)
      ans <- estimate_alpha_beta(x = x, size = size, prior_cases = prior_cases)
      expect_equal(ans[["alpha"]] / (ans[["alpha"]] + ans[["beta"]]), prob, tolerance = 0.05)
    }
  }
})
