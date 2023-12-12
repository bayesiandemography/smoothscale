
## 'check_x_size' ---------------------------------------------------

test_that("'check_x_size' returns TRUE with valid inputs", {
  expect_true(check_x_size(x = 1:6, size = 2:7, na_ok = FALSE))
  expect_true(check_x_size(x = c(1:6, NA), size = c(2:7, NA), na_ok = TRUE))
})

test_that("'check_x_size' raises correct error with non-numeric", {
  expect_error(check_x_size(x = 1:6, size = c("a", 3:7), na_ok = FALSE),
               "`size` is not numeric.")
})

test_that("'check_x_size' raises correct error with infinite", {
  expect_error(check_x_size(x = 1:6, size = c(Inf, 3:7), na_ok = FALSE),
               "`size` has infinite value.")
})

test_that("'check_x_size' raises correct error with negative", {
  expect_error(check_x_size(x = c(-1, 2:6), size = 2:7, na_ok = FALSE),
               "`x` has negative value.")
})

test_that("'check_x_size' raises correct error with NA", {
  expect_error(check_x_size(x = c(NA, 2:6), size = 2:7, na_ok = FALSE),
               "`x` has NA.")
})

test_that("'check_x_size' raises correct error with different lengths", {
  expect_error(check_x_size(x = 2:6, size = 2:7, na_ok = FALSE),
               "`x` and `size` have different lengths.")
})

test_that("'check_x_size' raises correct error with x > popn", {
  expect_error(check_x_size(x = c(2:6, 8), size = 2:7, na_ok = FALSE),
               "`x` greater than `size`.")
})


## 'check_prior_cases' --------------------------------------------------------

test_that("'check_prior_cases' returns TRUE with valid inputs", {
  expect_true(check_prior_cases(100))
  expect_true(check_prior_cases(0))
})

test_that("'check_prior_cases' raises correct error with non-numeric", {
  expect_error(check_prior_cases("a"),
               "`prior_cases` is not numeric.")
})

test_that("'check_prior_cases' raises correct error with length not equal to 1", {
  expect_error(check_prior_cases(c(100, 100)),
               "`prior_cases` has length 2.")
  expect_error(check_prior_cases(numeric()),
               "`prior_cases` has length 0.")
})

test_that("'check_prior_cases' raises correct error with infinite", {
  expect_error(check_prior_cases(Inf),
               "`prior_cases` is infinite.")
})

test_that("'check_prior_cases' raises correct error with NA", {
  expect_error(check_prior_cases(NA_real_),
               "`prior_cases` is NA.")
})

test_that("'check_prior_cases' raises correct error with negative", {
  expect_error(check_prior_cases(-1),
               "`prior_cases` is negative.")
})



## 'check_prob' ---------------------------------------------------------------

test_that("'check_prob' returns TRUE with valid inputs", {
  expect_true(check_prob(c(0.5, 0.5, 0.5), all_equal = TRUE, nm = "target"))
  expect_true(check_prob(0, nm = "target"))
})

test_that("'check_prob' returns TRUE with varying values if 'all_equal' is FALSE", {
  expect_true(check_prob(c(1, 1, 1, 0.2), all_equal = FALSE, nm = "unscaled"))
})

test_that("'check_prob' raises correct error with non-numeric", {
  expect_error(check_prob("a", all_equal = TRUE, nm = "target"),
               "`target` is not numeric.")
})

test_that("'check_prob' raises correct error with NA", {
  expect_error(check_prob(NA_real_, all_equal = TRUE, nm = "target"),
               "`target` has NAs.")
})

test_that("'check_prob' raises correct error with negative", {
  expect_error(check_prob(-1, all_equal = TRUE, nm = "target"),
               "`target` has negative values.")
})

test_that("'check_prob' raises correct error with greater than 1", {
  expect_error(check_prob(2, all_equal = TRUE, nm = "target"),
               "`target` has values greater than 1.")
})

test_that("'check_prob' raises correct error with length 0", {
  expect_error(check_prob(numeric(), all_equal = TRUE, nm = "target"),
               "`target` has length 0.")
})

test_that("'check_prob' raises correct error with unequal values", {
  expect_error(check_prob(c(1, 1, 1, 0.2), all_equal = TRUE, nm = "target"),
               "Element 4 of `target` \\(0.2\\) not equal to element 1 \\(1\\).")
})


## 'check_wt' ---------------------------------------------------------------

test_that("'check_wt' returns TRUE with valid inputs", {
  expect_true(check_wt(c(0.5, 0.5, 0.5), unscaled = rep(0.2, 3)))
})

test_that("'check_wt' raises correct error with non-numeric", {
  expect_error(check_wt("a", unscaled = 0.3),
               "`wt` is not numeric.")
})

test_that("'check_wt' raises correct error with NA", {
  expect_error(check_wt(NA_real_, unscaled = 0.3),
               "`wt` has NAs.")
})

test_that("'check_wt' raises correct error with negative", {
  expect_error(check_wt(-1, unscaled = 0.2),
               "`wt` has negative values.")
})

test_that("'check_wt' raises correct error with infinite", {
  expect_error(check_wt(Inf, unscaled = 0.3),
               "`wt` has infinite values.")
})

test_that("'check_wt' raises correct error with length not equal to unscaled", {
  expect_error(check_wt(0.5, unscaled = c(0.2, 0.3)),
               "`wt` and `unscaled` have different lengths.")
})
