test_that("maxi works", {
    maxir <- function(x) {
        res <- max(x, na.rm = TRUE)
        if (is.finite(res))
            res
        else NA_real_
    }

    x <- c(1.3, 134.4, 123.3, 1324.5)
    expect_equal(maxir(x), max(x))
    expect_equal(maxi(x), maxir(x))

    x <- c(1.3, 134.4, 123.3, 1324.5, NA)
    expect_equal(maxir(x), max(x, na.rm = TRUE))
    expect_equal(maxi(x), maxir(x))

    x <- c(NA_real_, NA_real_)
    expect_equal(maxir(x), NA_real_)
    expect_equal(maxir(x), maxi(x))

    x <- c(NA_real_, 1.3, 3.2, 1.4)
    expect_equal(maxi(x), 3.2)
    expect_equal(maxir(x), maxi(x))

    x <- c(1.4, 4.3, NA)
    expect_equal(maxi(x), 4.3)
    expect_equal(maxir(x), maxi(x))

    x <- c(NA_real_)
    expect_equal(maxi(x), maxir(x))
    expect_equal(maxi(x), NA_real_)

    expect_equal(maxi(numeric()), maxir(numeric()))
    expect_equal(maxi(numeric()), NA_real_)

    expect_equal(maxi(4), maxir(4))
    expect_equal(maxi(4), 4)
})
