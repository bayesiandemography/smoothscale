
test_that("'smooth_prob' works with valid inputs", {
    set.seed(0)
    size <- round(runif(n = 10000, min = 5, max = 20))
    x <- rbinom(n = 10000,
                size = size,
                prob = rbeta(n = 10000, shape1 = 10, shape2 = 20))
    prob <- smooth_prob(x = x, size = size)
    expect_true(var(prob) < var(x / size))
    expect_equal(mean(prob), 10 / (10 + 20), tolerance = 0.01)
})
