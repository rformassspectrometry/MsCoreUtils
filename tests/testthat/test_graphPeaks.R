## function shiftMatrix
library("testthat")

## create test matrices
mat_l <- matrix(letters[1:18], ncol = 6, nrow = 3)
## n: negative, p: positive
mat_n1 <- matrix(c("j", "p", NA, "k", "q", NA, "l", "r", NA), 
                 ncol = 3, nrow = 3, byrow = TRUE)
mat_n2 <- matrix(c("p", NA, NA, "q", NA, NA, "r", NA, NA), 
                 ncol = 3, nrow = 3, byrow = TRUE)
mat_p1 <- matrix(c(NA, "d", "j", NA, "e", "k", NA, "f", "l"), 
                 ncol = 3, nrow = 3, byrow = TRUE)
mat_p2 <- matrix(c(NA, NA, "d", NA, NA, "e", NA, NA, "f"), 
                 ncol = 3, nrow = 3, byrow = TRUE)

test_that("shiftMatrix", {
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = -1), mat_n1)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = -2), mat_n2)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = 1), mat_p1)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = 2), mat_p2)
    expect_error(shiftMatrix(x = c(2, 4, 6), n = 1, def = NA))
    expect_error(shiftMatrix(mat = mat_l, n = 1, def = NA))
    expect_error(shiftMatrix(mat = mat_l, x = c(2, 4, 6), def = NA))
    expect_error(shiftMatrix(mat = mat_l, x = c(2, 4, 6, 8, 10), n = 1, def = NA))
})


## function graphPeaks
library("testthat")
## create example spectrum1 and spectrum2 and perform tests
spectrum1 <- matrix(c(c(100.001, 100.002, 300.01, 300.02),
                      c(1, 1, 1, 1)), ncol = 2, nrow = 4, byrow = FALSE)
colnames(spectrum1) <- c("mz", "intensity")

spectrum2 <- matrix(c(c(100.0, 200.0, 300.002, 300.025, 300.0255),
                      c(1, 1, 1, 1, 1)), ncol = 2, nrow = 5, byrow = FALSE)
colnames(spectrum2) <- c("mz", "intensity")

## create matrices that contain the result
spectrum1_match <- matrix(c(c(100.002, 100.001, NA, 300.01, 300.02), 
                            c(NA, 1, 1, 0, 1, 1, 0)), ncol = 2, nrow = 6, byrow = FALSE)
colnames(spectrum1_match) <- c("mz", "intensity")

spectrum2_match <- matrix(c(c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
                            c(1, 0, 1, 1, 1, 1)), ncol = 2, nrow = 6, byrow = FALSE)

test_that("graphPeaks", {
    expect_equal(graphPeaks(x = spectrum1, y = spectrum1), list(x = spectrum1, y = spectrum1))
    expect_equal(graphPeaks(x = spectrum2, y = spectrum2), list(x = spectrum2, y = spectrum2))
    expect_error(graphPeaks(x = spectrum1[1, ], y = spectrum2))
    expect_error(graphPeaks(x = spectrum1))
    expect_error(graphPeaks(y = spectrum2))
    expect_error(graphPeaks(x = spectrum1, y = spectrum2, FUN = max))
})
