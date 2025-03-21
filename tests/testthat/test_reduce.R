test_that("reduce works", {
    s <- c(12.4, 454.2, 42.1, 15.3, 17.1, 34.4, 26.8, 11.4, 123.5, 599.1)
    e <- c(13.4, 490.1, 43.2, 17.2, 18.5, 39.1, 35.3, 16.1, 143.2, 699.2)
    res <- reduce(s, e)
    expect_equal(res, list(c(11.4, 26.8, 42.1, 123.5, 454.2, 599.1),
                           c(18.5, 39.1, 43.2, 143.2, 490.1, 699.2)))
    expect_equal(reduce(), list(numeric(), numeric()))
    expect_error(reduce(1:3, 1:5), "same length")
    expect_error(reduce(2:4, 1:3), "smaller or equal")

    ## same start
    s <- c(3.1, 3.1)
    e <- c(4.1, 6.1)
    res <- reduce(s, e)
    expect_equal(res[[1L]], 3.1)
    expect_equal(res[[2L]], 6.1)

    ## same end
    s <- c(1.2, 3.2)
    e <- c(4.0, 4.0)
    res <- reduce(s, e)
    expect_equal(res[[1L]], 1.2)
    expect_equal(res[[2L]], 4.0)

    ## start matches end
    s <- c(1.1, 2.2, 3.3)
    e <- c(2.2, 3.3, 4.4)
    res <- reduce(s, e)
    expect_equal(res[[1L]], 1.1)
    expect_equal(res[[2L]], 4.4)

    s <- c(12.1, 13, 15)
    e <- c(13, 14.5, 16)
    res <- reduce(s, e)
    expect_equal(res[[1L]], c(12.1, 15.0))
    expect_equal(res[[2L]], c(14.5, 16.0))

})
