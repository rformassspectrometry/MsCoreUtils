test_that("vapply", {
    l <- list(foo=c("a", "b"), bar="c")
    expect_equal(vapply1c(l, "[[", 1), c("a", "c"))

    l <- list(a=1:3, b=4:6)
    expect_equal(vapply1d(l, sum), c(6, 15))
    expect_equal(vapply1l(l, function(x)any(x > 5)), c(FALSE, TRUE))
})
