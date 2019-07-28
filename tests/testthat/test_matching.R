test_that("closest throws errors", {
    expect_error(closest(1, c(0, -1, 3)), "sorted non-decreasingly")
    expect_error(closest(), "missing, with no default")
    expect_error(closest(1:3, 1:3, tolerance = -1), "larger or equal zero")
    expect_error(closest(1:3, 1:3, tolerance = c(1, -1)), "larger or equal zero")
    expect_error(closest(1:3, 1.3, tolerance = TRUE), "numeric")
    expect_error(closest(1:3, 1:3, nomatch = TRUE), "be a 'numeric'")
    expect_error(closest(1, 1, nomatch = 1:2),
                 "'nomatch' has to be a 'numeric' of length one")
})

test_that("closest basically works", {
    expect_equal(closest(c(1.4, 9.8, 11.1), 1:10), c(1, 10, 10))
    expect_equal(closest(4:5, 4.8, tolerance = 1), c(1, 1))
    expect_equal(closest(c(0.5, 1.5, exp(1), pi), 1:10), c(1, 1, 3, 3))
})

test_that("closest, length(table) == 1, no tolerance", {
    expect_equal(closest(1:3, 0, nomatch = 0, tolerance = 0), c(0, 0, 0))
    expect_equal(closest(1:3, 1, nomatch = 0, tolerance = 0), c(1, 0, 0))
    expect_equal(closest(1:3, 2, nomatch = 0, tolerance = 0), c(0, 1, 0))
    expect_equal(closest(1:3, 3, nomatch = 0, tolerance = 0), c(0, 0, 1))
    expect_equal(closest(1:3, 4, nomatch = 0, tolerance = 0), c(0, 0, 0))
})

test_that("closest, tolerance", {
    expect_equal(closest(1.001, 1:10, tolerance = 0), NA_integer_)
    expect_equal(closest(1.4, 1:10, tolerance = 0.4), 1)

    # exact boundary, see
    # https://github.com/rformassspectrometry/Spectra/pull/45#issuecomment-511680248
    x <- c(1.11, 45.02, 123.45, 556.45)
    y <- c(3.01, 34.12, 45.021, 46.1, x[3] + (x[3] * 5 / 1e6), 556.449)

    expect_equal(closest(x, y, tolerance = 0.01), c(NA, 3L, 5L, 6L))

    # upper boundary
    expect_equal(closest(x, y, tolerance = y * 5 / 1e6), c(NA, NA, 5, 6))
    # lower boundary
    y <- c(3.01, 34.12, 45.021, 46.1, x[3] - (x[3] * 5 / 1e6), 556.449)
    expect_equal(closest(x, y, tolerance = y * 5 / 1e6), c(NA, NA, 5, 6))
})

test_that("closest, duplicates", {
    expect_equal(closest(c(0.8, 1.2), 1, tolerance = 0.3, duplicates = "keep"),
                 c(1, 1))
    expect_equal(closest(c(0.8, 1.2), 1, tolerance = 0.3, duplicates = "closest"),
                 c(1, NA_integer_))
    expect_equal(closest(c(0.8, 1.1), 1, tolerance = 0.3, duplicates = "closest"),
                 c(NA_integer_, 1))
    expect_equal(closest(c(0.8, 1.2), 1, tolerance = 0.3, duplicates = "remove"),
                 c(NA_integer_, NA_integer_))

    expect_equal(closest(1.5, 1:2, tolerance = 0.5, duplicates = "keep"), 1)
    expect_equal(closest(1.5, 1:2, tolerance = 0.5, duplicates = "closest"), 1)
    expect_equal(closest(1.6, 1:2, tolerance = 0.5, duplicates = "keep"), 2)
    expect_equal(closest(1.6, 1:2, tolerance = 0.5, duplicates = "closest"), 2)
    expect_equal(closest(c(NA, 1.6), 1:2, tolerance = 0.5,
                         duplicates = "closest"), c(NA, 2))
    expect_equal(closest(1.5, 1:2, tolerance = 0.5, duplicates = "remove"),
                 NA_integer_)
})

test_that("common", {
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5), rep(TRUE, 3))
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5, duplicates =
                        "closest"), c(FALSE, FALSE, TRUE))
    expect_equal(common(c(1.6, 1.75, 5.8), 1:2, tolerance = 0.5, duplicates =
                        "closest"), c(FALSE, TRUE, FALSE))
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5, duplicates =
                        "remove"), rep(FALSE, 3))
})

test_that("groupRun works", {
    x <- 1:6
    res <- groupRun(x)
    expect_equal(length(x), length(res))
    expect_identical(res, 1:6)
    res <- groupRun(x, tolerance = 1)
    expect_equal(length(x), length(res))
    expect_true(all(res == 1))

    x[5] <- x[4] + ppm(x[4], 5)
    res <- groupRun(x, ppm = 5)
    expect_equal(length(x), length(res))
    expect_equal(res, c(1, 2, 3, 4, 4, 5))
    x[6] <- x[5] + ppm(x[5], 3)
    res <- groupRun(x, ppm = 5)
    expect_equal(length(x), length(res))
    expect_equal(res, c(1, 2, 3, 4, 4, 4))
})

test_that("joinNumeric works", {
    x <- c(12.2, 13.4, 16.5, 23.5, 45.3, 57, 87)
    y <- c(5.3, 8.9, 13.4 + ppm(13.4, 5), 34.5, 45.3 + ppm(45.3, 10), 57.1, 234.2)

    res <- joinNumeric(x, y)
    expect_true(length(res$x) == length(res$y))
    expect_true(length(res$x) == length(c(x, y)))

    res <- joinNumeric(x, y, join = "left")
    expect_true(length(res$x) == length(x))
    expect_equal(res$x, seq_along(x))
    expect_equal(res$y, rep(NA_integer_, length(x)))

    res <- joinNumeric(x, y, join = "right")
    expect_true(length(res$x) == length(y))
    expect_equal(res$x, rep(NA_integer_, length(y)))
    expect_equal(res$y, seq_along(y))

    res <- joinNumeric(x, y, join = "inner")
    expect_true(length(res$x) == length(res$y))
    expect_true(length(res$x) == 0)

    res <- joinNumeric(x, y, ppm = 5)
    expect_true(length(res$x) == length(res$y))
    expect_true(length(res$x) == length(c(x, y)) - 1) # one element matching

    res <- joinNumeric(x, y, ppm = 5, join = "inner")
    expect_true(length(res$x) == length(res$y))
    expect_equal(res$x, 2)
    expect_equal(res$y, 3)

    res <- joinNumeric(x, y, ppm = 10, join = "inner")
    expect_true(length(res$x) == length(res$y))
    expect_equal(res$x, c(2, 5))
    expect_equal(res$y, c(3, 5))

    res <- joinNumeric(x, y, tolerance = 0.1, join = "inner")
    expect_true(length(res$x) == length(res$y))
    expect_equal(res$x, c(2, 5, 6))
    expect_equal(res$y, c(3, 5, 6))
})
