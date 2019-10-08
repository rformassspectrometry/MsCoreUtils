test_that(".orderEdges", {
    x <- c(100.1, 100.2, 300, 500)
    y <- c(100, 200, 300.1)
    e <- list(x = c(1, 2, 3, 4, NA), y = c(1, 1, 3, NA, 2))
    o <- list(x = c(1, 2, NA, 3, 4), y = c(1, 1, 2, 3, NA))

    expect_equal(.orderEdges(x, y, e), o)
})
