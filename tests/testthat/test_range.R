test_that("between throws errors", {
    expect_error(between("A", 1:2), "numeric")
    expect_error(between(1, c("A", "B")), "numeric of length 2")
    expect_error(between(1, 1:4), "numeric of length 2")
})

test_that("between", {
    expect_equal(between(1:4, 2:3), c(FALSE, TRUE, TRUE, FALSE))
    expect_equal(between(1:4, 3:2), c(FALSE, TRUE, TRUE, FALSE))
    expect_equal(1:4 %between% 2:3, c(FALSE, TRUE, TRUE, FALSE))
})
