
test_that("'smooth_count' works with valid inputs", {
    set.seed(0)
    size <- round(runif(n = 100, min = 5, max = 20))
    count <- rbinom(n = 100,
                    size = size,
                    prob = rbeta(n = 100, shape1 = 10, shape2 = 20))
    smoothed <- smooth_count(count = count, size = size)
    expect_true(var(smoothed) < var(count))
})
