s <- list(mass=1:20, intensity=rep(10:1, 2))

test_that("estimateBaseline throws errors", {
    expect_error(estimateBaseline(s$mass,
                                  s$intensity,
                                  method = "foobar"),
                 ".*arg.* should be one of .*SNIP.*, .*ConvexHull.*, .*median.*")
    expect_error(estimateBaseline(s$mass[-1], s$intensity),
                 "Mass and intensity vectors must be of same lengths.")
})

test_that("estimateBaselineConvexHull", {
    b <- c(10:1, rep(1, 10))
    expect_identical(
        estimateBaselineConvexHull(s$mass, s$intensity),
        b)
    ## user method
    expect_identical(
        estimateBaseline(s$mass, s$intensity, method = "ConvexHull"),
        b)
})

test_that("estimateBaselineMedian", {
    b <- c(10:4, rep(c(3, 8), each = 3), 7:1)
    b1 <- c(10:3, rep(c(2, 9), each = 2), 8:1)
    expect_identical(
        estimateBaselineMedian(s$mass, s$intensity, 1),
        b1)
    expect_identical(
        estimateBaselineMedian(s$mass, s$intensity, 2),
        b)
    ## user method
    expect_identical(
        estimateBaseline(s$mass, s$intensity,
                         method = "median", halfWindowSize = 1),
        b1)
    expect_identical(
        estimateBaseline(s$mass, s$intensity,
                         method = "median", halfWindowSize = 2),
        b)
    ## halfWindowSize
    expect_error(estimateBaselineMedian(s$mass, s$intensity, 0),
                 "too small")
    expect_error(estimateBaselineMedian(s$mass, s$intensity, 20),
                 "too large")
})

test_that("estimateBaselineSnip", {
    b <- list(
        decreasing = list(b = c(10:1, c(2.5, 4.0, 5.5), 7:1),
                          b100 = c(10:1, rep(c(1.25, 1.5, 1.75, 1.375, 1),
                                             times = 2))),
        increasing = list(b = c(10:1, 5:4, 5.5, 7:1),
                          b100 = c(10:1, 3.75, 3.375, 3:1, 2.5, 4:1)))
    ## test default decreasing argument
    expect_identical(
        estimateBaselineSnip(s$mass, s$intensity, 2),
        b$decreasing$b)
    for (j in seq(along = b)) {
        d <- names(b)[j] == "decreasing"

        expect_identical(
            estimateBaselineSnip(s$mass, s$intensity,
                                                2, decreasing = d),
            b[[j]]$b)
        expect_equal(
            estimateBaselineSnip(s$mass, s$intensity,
                                                decreasing = d),
            b[[j]]$b100)
        ## user method
        expect_identical(
            estimateBaseline(s$mass, s$intensity,
                             method = "SNIP", iterations = 2,
                             decreasing = d),
            b[[j]]$b)
        expect_identical(
            estimateBaseline(s$mass, s$intensity,
                             iterations = 2, decreasing = d),
            b[[j]]$b)
        expect_equal(
            estimateBaseline(s$mass, s$intensity,
                             decreasing = d),
            b[[j]]$b100)
    }
})

test_that("estimateBaselineTopHat", {
    b <- c(rep(8, 3), 7:1, rep(6, 5), 5:1)
    b1 <- c(rep(9, 2), 8:1, rep(8, 3), 7:1)
    expect_identical(
        estimateBaselineTopHat(s$mass, s$intensity, 1),
        b1)
    expect_identical(
        estimateBaselineTopHat(s$mass, s$intensity, 2),
        b)
    ## user method
    expect_identical(
        estimateBaseline(s$mass, s$intensity,
                         method = "TopHat",
                         halfWindowSize = 2),
        b)
    ## halfWindowSize
    expect_error(
        estimateBaselineTopHat(s$mass, s$intensity, 0),
        "too small")
    expect_error(
        estimateBaselineTopHat(s$mass, s$intensity, 20),
        "too large")
})
