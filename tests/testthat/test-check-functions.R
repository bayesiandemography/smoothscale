
## 'check_count_population' ---------------------------------------------------

test_that("'check_count_population' returns TRUE with valid inputs", {
    expect_true(check_count_population(count = 1:6, population = 2:7, na_ok = FALSE))
    expect_true(check_count_population(count = c(1:6, NA), population = c(2:7, NA), na_ok = TRUE))
})

test_that("'check_count_population' raises correct error with non-numeric", {
    expect_error(check_count_population(count = 1:6, population = c("a", 3:7), na_ok = FALSE),
                 "`population` is not numeric.")
})

test_that("'check_count_population' raises correct error with infinite", {
    expect_error(check_count_population(count = 1:6, population = c(Inf, 3:7), na_ok = FALSE),
                 "`population` has infinite value.")
})

test_that("'check_count_population' raises correct error with negative", {
    expect_error(check_count_population(count = c(-1, 2:6), population = 2:7, na_ok = FALSE),
                 "`count` has negative value.")
})

test_that("'check_count_population' raises correct error with NA", {
    expect_error(check_count_population(count = c(NA, 2:6), population = 2:7, na_ok = FALSE),
                 "`count` has NA.")
})

test_that("'check_count_population' raises correct error with different lengths", {
    expect_error(check_count_population(count = 2:6, population = 2:7, na_ok = FALSE),
                 "`count` and `population` have different lengths.")
})

test_that("'check_count_population' raises correct error with count > popn", {
    expect_error(check_count_population(count = c(2:6, 8), population = 2:7, na_ok = FALSE),
                 "`count` greater than `population`.")
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



## 'check_total' --------------------------------------------------------------

test_that("'check_total' returns TRUE with valid inputs", {
    expect_true(check_total(c(100, 100, 100)))
    expect_true(check_total(0))
})

test_that("'check_total' raises correct error with non-numeric", {
    expect_error(check_total("a"),
                 "`total` is not numeric.")
})

test_that("'check_total' raises correct error with infinite", {
    expect_error(check_total(Inf),
                 "`total` has infinite values.")
})

test_that("'check_total' raises correct error with NA", {
    expect_error(check_total(NA_real_),
                 "`total` has NAs.")
})

test_that("'check_total' raises correct error with negative", {
    expect_error(check_total(-1),
                 "`total` has negative values.")
})

test_that("'check_total' raises correct error with length 0", {
    expect_error(check_total(numeric()),
                 "`total` has length 0.")
})

test_that("'check_total' raises correct error with unequal values", {
    expect_error(check_total(c(1, 1, 1, 2)),
                 "Element 4 of `total` \\(2\\) not equal to element 1 \\(1\\).")
})




