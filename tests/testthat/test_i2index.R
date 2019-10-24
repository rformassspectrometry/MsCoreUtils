test_that("i2index works", {
    res <- i2index(3:4, 10)
    expect_equal(i2index(3L:4L, 10L), 3:4)
    expect_equal(i2index(3:4, 10), 3:4)
    expect_equal(i2index(-1, 10), -1)
    expect_error(i2index(4, 3), "has to be between 1 and 3")

    expect_error(i2index("a", 5), "object does not have names")
    expect_error(i2index(c("a", "d"), 3, names = letters[1:3]), "not all names")
    expect_equal(i2index(c("a", "d"), 4, names = letters[1:4]), c(1, 4))

    expect_error(i2index(c(TRUE, FALSE), 3), "has to match the length")
    expect_equal(i2index(rep(FALSE, 3), 3), integer())
    expect_equal(i2index(c(FALSE, TRUE, TRUE), 3), 2:3)
})
