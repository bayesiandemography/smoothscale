
test_that("'scale_prob' works when scaling up", {
  set.seed(0)
  prob_true <- 0.5
  scale <- 0.8
  n_area <- 100
  size <- round(runif(n = n_area, min = 5, max = 20))
  x <- rbinom(n = 100, size = size, prob = scale * prob_true)
  prob_report <- x / size
  ans <- scale_prob(prob_report = prob_report, prob_target = prob_true)
  expect_equal(mean(ans), prob_true, tolerance = 0.01)
  ans_wt <- scale_prob(prob_report = prob_report, prob_target = prob_true, wt = size)
  expect_equal(weighted.mean(ans_wt, w = size), prob_true)
  expect_true(all(ans >= 0))
  expect_true(all(ans <= 1))
  expect_true(all(ans >= x / size))
})

test_that("'scale_prob' works when scaling down", {
  set.seed(0)
  prob_true <- 0.5
  scale <- 1.2
  n_area <- 100
  size <- round(runif(n = n_area, min = 5, max = 20))
  x <- rbinom(n = 100, size = size, prob = scale * prob_true)
  prob_report <- x / size
  ans <- scale_prob(prob_report, prob_target = prob_true, wt = size)
  expect_equal(weighted.mean(ans, w = size), prob_true)
  expect_true(all(ans >= 0))
  expect_true(all(ans <= 1))
  expect_true(all(ans <= x / size))
})

