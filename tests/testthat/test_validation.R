test_that("validPeaksMatrix", {
    expect_error(validPeaksMatrix(1:2), "matrix")
    expect_error(validPeaksMatrix(matrix(1:3, nrow = 1)), "two columns")
    expect_error(validPeaksMatrix(matrix(1:2, ncol = 2)), "'mz' and 'intensity'")
    expect_error(validPeaksMatrix(cbind(foo = 1, bar = 2)), "'mz' and 'intensity'")
    expect_error(validPeaksMatrix(cbind(mz = "A", intensity = "B")), "'numeric'")
    expect_error(validPeaksMatrix(cbind(mz = 2:1, intensity = 2:1)), "sorted")
    expect_true(validPeaksMatrix(cbind(mz = 1:2, intensity = 1:2)))
})
