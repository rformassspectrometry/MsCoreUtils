test_that("isPeaksMatrix", {
    expect_false(isPeaksMatrix(1:2))
    expect_false(isPeaksMatrix(matrix(1:3, nrow = 1)))
    expect_false(isPeaksMatrix(matrix(1:2, ncol = 2)))
    expect_false(isPeaksMatrix(cbind(foo = 1, bar = 2)))
    expect_false(isPeaksMatrix(cbind(mz = "A", intensity = "B")))
    expect_true(isPeaksMatrix(cbind(mz = 1:2, intensity = 1:2)))
})
