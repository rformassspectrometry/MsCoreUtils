test_that("noise", {
    expect_error(noise(1, 1, "foo"), "MAD")
    expect_error(noise("a", "b"))

    x <- 1:20
    y <- c(1:10, 10:1)
    expect_identical(noise(x, y), rep(mad(y), 20))
    expect_identical(noise(x, y, "SuperSmoother", span = 1 / 3),
                     supsmu(x, y, span = 1 / 3)$y)
})
