test_that("smooth throws errors", {
    expect_error(smooth(1:10, 1), "matrix")
    expect_error(smooth(1:10, matrix(1:10, nrow = 2, ncol = 5)), "rows")
})

test_that("smooth", {
    # (Weighted)MovingAverage
    r <- c(rep(3, 3), 4:7, rep(8, 3))
    expect_null(attributes(smooth(1:10, coefMA(2))))
    expect_equal(smooth(1:10, coefMA(2)), r)
    expect_equal(smooth(1:10, coefWMA(2)), r)

    x <- c(rep(1, 4), 6, 20, 6, rep(1, 4))
    expect_equal(smooth(x, coefMA(2)),
                 c(rep(c(2, 5.8, 6.8, 5.8, 2), c(3, 1, 3, 1, 3))))
    expect_equal(smooth(x, coefWMA(2)),
                 c(rep(1.5, 3), 3.9, 7.3, 10.6, 7.3, 3.9, rep(1.5, 3)))

    # SavitzkyGolay
    expect_equal(smooth(1:10, coefMA(2)), smooth(1:10, coefSG(2, 0L)))

    x <- c(8L, 1L, 7L, 6L, 3L, 13L, 5L, 2L, 19L, 11L, 15L, 18L, 9L, 10L, 20L,
           12L, 17L, 14L, 16L, 4L)

    r <- c(7.64285714285716, 2.42857142857143, 4.85714285714286,
           5.14285714285714, 6.94285714285714, 8.37142857142857,
           5.68571428571428, 7.14285714285714, 11.9714285714286,
           15.2857142857143, 14.8285714285714, 15.1714285714286,
           10.9714285714286, 12.2285714285714, 15.0285714285714,
           16.4571428571429, 14.0857142857143, 16.7428571428571,
           14.1714285714286, 4.45714285714283)
    expect_equal(smooth(x, coefSG(2)), r)
})

test_that("coefMA", {
    expect_equal(coefMA(1), matrix(1/3, 3, 3))
    expect_equal(coefMA(2), matrix(1/5, 5, 5))
})

test_that("coefWMA", {
    expect_equal(coefWMA(1), matrix(c(0.25, 0.5, 0.25), 3, 3, byrow = TRUE))
    expect_equal(coefWMA(2), matrix(c(0.1, 0.2, 0.4, 0.2, 0.1), 5, 5,
                                      byrow = TRUE))
})

test_that("coefSG throws errors", {
    expect_error(coefSG(2, 1), "integer")
    expect_error(coefSG(2, -1), "larger")
    expect_error(coefSG(2, 1L:2L), "length")
    expect_error(coefSG(2, 10L), "larger than the polynomial order")
})

test_that("coefSG", {
    expect_equal(coefSG(2, k = 0L), coefMA(2))

    r <- list(rep(1, 5)/5,
              c(-3, 12, 17, 12, -3)/35,
              c(-2, 3, 6, 7, 6, 3, -2)/21,
              c(-21, 14, 39, 54, 59, 54, 39, 14, -21)/231,
              c(5, -30, 75, 131, 75, -30, 5)/231,
              c(15, -55, 30, 135, 179, 135, 30, -55, 15)/429)
    hws <- c(2, 2, 3, 4, 3, 4)
    k <- c(0L, 2L, 3L, 3L:5L)

    for (i in seq(along = r)) {
        expect_equal(coefSG(hws[i], k[i])[hws[i] + 1, ], r[[i]])
    }
})
