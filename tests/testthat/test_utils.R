test_that(".combinations", {
    expect_error(
        .combinations(rep(1:100, each = 2)),
        "too many possible combinations"
    )

    g <- c(1, 2, 2, 2, 3, 3)
    l <- list(
        c(1, 2, 5), c(1, 3, 5), c(1, 4, 5),
        c(1, 2, 6), c(1, 3, 6), c(1, 4, 6)
    )

    expect_equal(.combinations(g), l)
})

test_that(".is{Precursor,Follower}Identical", {
    x <- c(1, 1, NA, 3, 4, 4, 5, 6, 6, 6, NA, 7, 8, 8)
    expect_equal(.isFollowerIdentical(x), c(
        TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
        TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
    )
    expect_equal(.isPrecursorIdentical(x), c(
        FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
        TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
    )
})

test_that(".transposeList", {
    l <- list(a = 1:10, b = 11:20, c = 21:30)
    r <- mapply(c, 1:10, 11:20, 21:30, SIMPLIFY = FALSE)

    expect_error(.transposeList(list(a = 1:3, b = 1:10)), "length")
    expect_equal(.transposeList(l), r)
})

test_that(".validateWindow", {
    expect_error(.validateWindow(3, 10L), "integer")
    expect_error(.validateWindow(3L:4L, 10L), "length")
    expect_error(.validateWindow(-1L, 10L), "larger")
    expect_error(.validateWindow(11L, 10L), "smaller")
    expect_true(.validateWindow(3L, 10L))
})
