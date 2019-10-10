test_that(".combinations", {
    g <- c(1, 2, 2, 2, 3, 3)
    l <- list(
        c(1, 2, 5), c(1, 3, 5), c(1, 4, 5),
        c(1, 2, 6), c(1, 3, 6), c(1, 4, 6)
    )

    expect_equal(.combinations(g), l)
})

test_that(".anyCrossing", {
    expect_false(.anyCrossing(list(x = 1:3, y = c(NA, 1:2))))
    expect_true(.anyCrossing(list(x = 1:3, y = c(2, 1, NA))))
})

test_that(".edgeGroups", {
    e1 <- list(x = c(1, 2, NA, 3, 4, 4, 5), y = c(1, 1, 2, 3, 3, 4, 4))
    e2 <- list(x = e1$y, y = e1$x)
    g <- c(1, 1, 2, 3, 3, 4, 4)

    expect_error(.edgeGroups(list(x = 1, y = 1:2)), "length")
    expect_equal(.edgeGroups(e1), g)
    expect_equal(.edgeGroups(e2), g)
})

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

test_that(".transposeList", {
    l <- list(a = 1:10, b = 11:20, c = 21:30)
    r <- mapply(c, 1:10, 11:20, 21:30, SIMPLIFY = FALSE)

    expect_error(.transposeList(list(a = 1:3, b = 1:10)), "length")
    expect_equal(.transposeList(l), r)
})
