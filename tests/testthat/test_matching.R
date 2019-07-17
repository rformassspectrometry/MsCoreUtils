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
    expect_equal(closest(1.5, 1:2, tolerance = 0.5, duplicates = "remove"),
                 NA_integer_)
})

test_that("common", {
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5), rep(TRUE, 3))
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5, duplicates =
                        "closest"), c(FALSE, FALSE, TRUE))
    expect_equal(common(c(1.6, 1.75, 1.8), 1:2, tolerance = 0.5, duplicates =
                        "remove"), rep(FALSE, 3))
})
