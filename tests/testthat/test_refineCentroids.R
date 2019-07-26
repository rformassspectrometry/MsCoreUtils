test_that("refineCentroids throws errors on wrong input", {
    expect_error(refineCentroids(TRUE), "numeric")
    expect_error(refineCentroids(1:3, TRUE), "numeric")
    expect_error(refineCentroids(1:3, 1:5), "same length")
    expect_error(refineCentroids(1:3, 1:3, p = 1), "integer")
    expect_error(refineCentroids(1:3, 1:3, p = 1L, k = TRUE), "integer")
    expect_error(refineCentroids(1:3, 1:3, p = 1L, k = 1:2), "length 1")
    expect_error(refineCentroids(1:3, 1:3, p = 1L, k = 2L, threshold = -0.5),
                 "between 0 and 1")
    expect_error(refineCentroids(1:3, 1:3, p = 1L, k = 2L, threshold = 1.5),
                 "between 0 and 1")
    expect_error(refineCentroids(1:3, 1:3, p = 1L, k = 2L, descending = NA),
                 "'TRUE' or 'FALSE'")
})

test_that("refineCentroids", {
    expect_equal(refineCentroids(1:3, 1:3), 1:3)
    expect_equal(refineCentroids(1:3, 1:3, p = integer()), 1:3)

    y <- c(1:5, 4:2, 1:3, 1:10, 9)
    x <- seq_along(y)
    p <- c(5L, 11L, 21L)

    expect_equal(refineCentroids(x, y, p, k = 2L,
                                 descending = FALSE, threshold = 0),
                 sapply(p, function(pp) {
                    i <- seq(from = -2, to = 2) + pp
                    weighted.mean(x[i], y[i], na.rm = TRUE)
                 })
    )

    expect_equal(refineCentroids(x, y, p, k = 2L,
                                 descending = FALSE, threshold = 0.5),
                 sapply(p, function(pp) {
                    i <- seq(from = -2, to = 2) + pp
                    i <- i[y[i]/y[pp] > 0.5]
                    weighted.mean(x[i], y[i], na.rm = TRUE)
                 })
    )

    expect_equal(refineCentroids(x, y, p, k = 10L,
                                 descending = TRUE, threshold = 0),
                 sapply(list(1:9, 9:12, 12:22), function(i) {
                    weighted.mean(x[i], y[i], na.rm = TRUE)
                 })
    )
})

test_that(".peakRegionMask", {
    y <- c(1:5, 4:2, 1:3, 1:10, 9)
    p <- c(5L, 11L, 21L)
    expect_equal(.peakRegionMask(y, p, k = 1),
                 matrix(rep(1, 9), nrow = 3))
    expect_equal(.peakRegionMask(y, p, k = 2),
                 matrix(c(rep(1, 5),
                          1, 1, 1, 1, 0,
                          1, 1, 1, 1, 0), nrow = 5))
    expect_equal(.peakRegionMask(y, p, k = 3),
                 matrix(c(rep(1, 7),
                          0, 1, 1, 1, 1, 0, 0,
                          1, 1, 1, 1, 1, 0, 0), nrow = 7))
})
