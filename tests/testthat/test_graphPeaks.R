## function shiftMatrix
library("testthat")

test_that("shiftMatrix", {
    
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
    
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = -1), mat_n1)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = -2), mat_n2)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = 1), mat_p1)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = 2), mat_p2)
    expect_equal(shiftMatrix(mat_l, x = c(2, 4, 6), n = 0), mat_l[, c(2, 4, 6)])
    expect_error(shiftMatrix(x = c(2, 4, 6), n = 1, def = NA))
    expect_error(shiftMatrix(mat = mat_l, n = 1, def = NA))
    expect_error(shiftMatrix(mat = mat_l, x = c(2, 4, 6), def = NA))
    expect_error(shiftMatrix(mat = mat_l, x = c(2, 4, 6, 8, 10), n = 1, def = NA))
})


## function graphPeaks
library("testthat")

## create example x and y and perform tests
x <- matrix(c(c(100.001, 100.002, 300.01, 300.02),
              c(1, 1, 1, 1)), ncol = 2, nrow = 4, byrow = FALSE)
colnames(x) <- c("mz", "intensity")

y <- matrix(c(c(100.0, 200.0, 300.002, 300.025, 300.0255),
              c(1, 1, 1, 1, 1)), ncol = 2, nrow = 5, byrow = FALSE)
colnames(y) <- c("mz", "intensity")

## create matrices that contain the result
x_match <- matrix(c(c(100.002, 100.001, NA, 300.01,  300.02, NA), 
                    c(1, 1, 0, 1, 1, 0)), ncol = 2, nrow = 6)
colnames(x_match) <- c("mz", "intensity")

y_match <- matrix(c(c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
                    c(1, 0, 1, 1, 1, 1)), ncol = 2, nrow = 6)
colnames(y_match) <- c("mz", "intensity")

test_that("graphPeaks", {
    ## tests
    expect_equal(graphPeaks(x = x, y = x), list(x = x, y = x),
        tolerance = 1e-05)
    expect_equal(graphPeaks(x = y, y = y), list(x = y, y = y), 
        tolerance = 1e-05)
    expect_equal(graphPeaks(x = x, y = y, m = 0.5, n = 0)$x, x_match, tolerance = 1e-05)
    expect_equal(graphPeaks(x = x, y = y, m = 0.5, n = 0)$y, y_match, tolerance = 1e-05)
    expect_true(is.list(graphPeaks(x = x, y = y)))
    expect_error(graphPeaks(x = x[1, ], y = y))
    expect_error(graphPeaks(x = x, y = y[[1,]]))
    expect_error(graphPeaks(x = x))
    expect_error(graphPeaks(y = y))
    expect_error(graphPeaks(x = x, y = y, FUN = max))
    
    ## ppm
    expect_error(graphPeaks(x = x, y = y, ppm = "a"))
    expect_error(graphPeaks(x = x, y = y, ppm = -1))
    
    ## test for mode
    x_chr <- matrix(c("a", "b", "c", "d"), ncol = 2)
    expect_error(graphPeaks(x = x_chr, y = y))
    expect_error(graphPeaks(x = x, y = x_chr))
    
    ## test for colnames 
    x_colnames <- matrix(c(c(100.001, 100.002, 300.01, 300.02),
                  c(1, 1, 1, 1)), ncol = 2, nrow = 4, byrow = FALSE)
    expect_error(graphPeaks(x = x_colnames, y = y))
    expect_error(graphPeaks(x = y, y = x_colnames))

})
