test_that("localMaxima", {
    x <- c(1:5, 4:1, 1:10, 9:1, 1:5, 4:1)
    l <- logical(length(x))
    l[19] <- TRUE
    expect_identical(localMaxima(x, 10), l)
    l[c(5, 33)] <- TRUE
    expect_identical(localMaxima(x), l)
})
