
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



## 'check_prob_target' --------------------------------------------------------

test_that("'check_prob_target' returns TRUE with valid inputs", {
    expect_true(check_prob_target(c(0.5, 0.5, 0.5), nm = "prob_target"))
    expect_true(check_prob_target(0, nm = "prob_target"))
})

test_that("'check_prob_target' raises correct error with non-numeric", {
    expect_error(check_prob_target("a", nm = "prob_target"),
                 "`prob_target` is not numeric.")
})

test_that("'check_prob_target' raises correct error with NA", {
    expect_error(check_prob_target(NA_real_, nm = "prob_target"),
                 "`prob_target` has NAs.")
})

test_that("'check_prob_target' raises correct error with negative", {
    expect_error(check_prob_target(-1, nm = "prob_target"),
                 "`prob_target` has negative values.")
})

test_that("'check_prob_target' raises correct error with greater than 1", {
    expect_error(check_prob_target(2, nm = "prob_target"),
                 "`prob_target` has values greater than 1.")
})

test_that("'check_prob_target' raises correct error with length 0", {
    expect_error(check_prob_target(numeric(), nm = "prob_target"),
                 "`prob_target` has length 0.")
})

test_that("'check_prob_target' raises correct error with unequal values", {
    expect_error(check_prob_target(c(1, 1, 1, 0.2), nm = "prob_target"),
                 "Element 4 of `prob_target` \\(0.2\\) not equal to element 1 \\(1\\).")
})




