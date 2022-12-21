test_that("between throws errors", {
    expect_error(between(1, 1:4), "numeric of length 2")
})

test_that("between throws warnings", {
    expect_warning(between("A", 1:2), "NAs introduced")
    expect_warning(between(1, c("A", "B")), "NAs introduced")
})

test_that("between handles NA", {
    expect_equal(suppressWarnings(between("A", 1:2)), NA)
    expect_equal(suppressWarnings(between(1:3, c("A", "B"))), rep(NA, 3))
})

test_that("between", {
    expect_equal(between(1:4, 2:3), c(FALSE, TRUE, TRUE, FALSE))
    expect_equal(between(1:4, 3:2), c(FALSE, TRUE, TRUE, FALSE))
    expect_equal(1:4 %between% 2:3, c(FALSE, TRUE, TRUE, FALSE))
})
