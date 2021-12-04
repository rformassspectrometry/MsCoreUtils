test_that("which.first", {
    expect_error(which.first(1:3), "logical")
    expect_identical(
        which.first(c(NA, b = TRUE, c = TRUE, d = FALSE)),
        c(b = 2L)
    )
})

test_that("which.last", {
    expect_error(which.last(1:3), "logical")
    expect_identical(
        which.last(c(a = FALSE, b = TRUE, c = TRUE, d = NA)),
        c(c = 3L)
    )
})
