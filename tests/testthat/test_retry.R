test_that("retry works", {
    a <- function() {
        if (sample(0:1, 1) == 0)
            stop("A, got a 0")
        1
    }
    set.seed(123)
    ## With a seed of 123 `a()` throws an error 3 times and works on the 4th
    res <- retry(a(), ntimes = 5L)
    expect_equal(res, 1)
    ## Failure (fails 3 consecutive times)
    set.seed(123)
    expect_error(retry(a(), ntimes = 3L), "A, got a 0")

    ## Always fails
    expect_error(retry(hello, ntimes = 5L, retry_on = "not found"), "not found")
})
