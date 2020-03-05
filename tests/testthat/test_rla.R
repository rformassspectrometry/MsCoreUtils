test_that("rla and rowRla works", {
    x <- c(123, 545, 232, 1320, 4321, 150, 6531, 5422, 150)
    f <- c("b", "a", "b", "a", "a", "b", "a", "a", "b")

    expect_error(rla(x, f = c(1, 2)), "length of 'x'")
    res <- rla(x)
    expect_true(is.numeric(res))
    expect_true(length(res) == length(x))
    expect_equal(res, rla(log2(x), transform = "identity"))
    expect_error(rla(x, transform = "sqrt"))
    res <- rla(x, f = factor(f))
    expect_true(is.numeric(res))
    expect_true(length(res) == length(x))
    a <- c(545, 1320, 4321, 6531, 5422)
    b <- c(123, 232, 150, 150)
    expect_equal(res,
                 c(log2(a) - median(log2(a)), log2(b) - median(log2(b)))[
                     c(6, 1, 7, 2, 3, 8, 4, 5, 9)
                 ])
    res2 <- rla(x, f = f)
    expect_identical(res, res2)

    X <- rbind(x, x * 4, x * 10)
    res <- rowRla(X)
    expect_true(is.matrix(res))
    expect_true(nrow(res) == nrow(X))
    expect_true(ncol(res) == ncol(X))
    expect_equal(res[1, ], res[2, ])

    res <- rowRla(X, f = f)
    res2 <- rowRla(X, f = factor(f))
    expect_identical(res, res2)
    expect_identical(res[1, ], rla(x, f = f))
})
