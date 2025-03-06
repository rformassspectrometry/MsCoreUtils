test_that("reduce works", {
    s <- c(12.4, 454.2, 42.1, 15.3, 17.1, 34.4, 26.8, 11.4, 123.5, 599.1)
    e <- c(13.4, 490.1, 43.2, 17.2, 18.5, 39.1, 35.3, 16.1, 143.2, 699.2)
    res <- reduce(s, e)
    expect_equal(res, list(c(11.4, 26.8, 42.1, 123.5, 454.2, 599.1),
                           c(18.5, 39.1, 43.2, 143.2, 490.1, 699.2)))
    expect_equal(reduce(), list(numeric(), numeric()))
    expect_error(reduce(1:3, 1:5), "same length")
    expect_error(reduce(2:4, 1:3), "smaller or equal")
})
