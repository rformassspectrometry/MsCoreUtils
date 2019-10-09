test_that(".edgeList", {

    x <- c(100.1, 100.2, 300, 500)
    y <- c(100, 200, 300.1)
    e <- list(x = c(1, 2, 3, 4, NA), y = c(1, 1, 3, NA, 2))

    expect_equal(.edgeList(x, y, tolerance = 0.2, ppm = 0), e)
})

test_that(".orderEdges", {
    x <- c(100.1, 100.2, 300, 500)
    y <- c(100, 200, 300.1)
    e <- list(x = c(1, 2, 3, 4, NA), y = c(1, 1, 3, NA, 2))
    o <- list(x = c(1, 2, NA, 3, 4), y = c(1, 1, 2, 3, NA))

    expect_equal(.orderEdges(x, y, e), o)
})

test_that(".anyCrossing", {
    expect_false(.anyCrossing(list(x = 1:3, y = c(NA, 1:2))))
    expect_true(.anyCrossing(list(x = 1:3, y = c(2, 1, NA))))
})
