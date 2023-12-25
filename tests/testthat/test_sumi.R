test_that("sumi works", {
    sumir <- function(x) {
        if (all(is.na(x)))
            NA_real_
        else sum(x, na.rm = TRUE)
    }

    x <- c(3.2, 45.2, 1.2, NA)
    expect_equal(sumi(x), sumir(x))

    x <- numeric()
    expect_equal(sumi(x), NA_real_)

    x <- c(NA_real_, NA_real_)
    expect_equal(sumi(x), NA_real_)
})