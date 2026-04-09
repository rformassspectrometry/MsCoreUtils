test_that("retry works", {
    a <- function() {
        if (sample(0:1, 1) == 0)
            stop("A, got a 0")
        1
    }
    set.seed(123)
    ## With a seed of 123 `a()` throws an error 3 times and works on the 4th
    res <- retry(a(), ntimes = 5L, verbose = TRUE)
    expect_equal(res, 1)
    ## Failure (fails 3 consecutive times)
    set.seed(123)
    expect_error(retry(a(), ntimes = 3L, verbose = TRUE), "A, got a 0")

    ## Always fails
    expect_error(retry(hello, ntimes = 5L, retry_on = "not found",
                       verbose = TRUE), "not found")

    ## warnings as errors
    my_fun <- function(x) {
        warning("hello there")
        x * x
    }
    expect_warning(res <- retry(my_fun(3), ntimes = 5L, verbose = TRUE),
                   "hello there")
    expect_equal(res, 9)
    w <- getOption("warn")
    expect_error(retry(my_fun(3), ntimes = 5L, verbose = TRUE,
                       warningsAsErrors = TRUE), "hello there")
    expect_equal(getOption("warn"), w)


    expect_error(retry(my_fun(3), ntimes = 5L, verbose = TRUE,
                       retry_on = "hello",
                       warningsAsErrors = TRUE), "hello")

    ## connection errors.
    expect_warning(expect_error(
        retry(readLines("https://www.goog.el"), verbose = TRUE), "cannot"),
        "connect to server")
    expect_warning(expect_error(
        retry(readLines("https://www.goog.el"), verbose = TRUE,
              retry_on = "connect to"), "cannot"),
        "connect to server")
    expect_error(
        retry(readLines("https://www.goog.el"), verbose = TRUE,
              warningsAsErrors = TRUE, retry_on = "connect to"),
        "connect to server")
})
