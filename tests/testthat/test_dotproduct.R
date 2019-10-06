test_that("dotproduct", {
    
    ## create two dummy spectra (aligned)
    x <- matrix(c(c(100.002, 100.001, NA, 300.01, 300.02, NA), 
             c(2, 1.5, 0, 1.2, 0.9, 0)), ncol = 2)
    y <- matrix(c(c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
             intensity = c(2, 0, 3, 1, 4, 0.4)), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    
    ## calculate similarity values
    expect_warning(dotproduct(x, y, m = 0, n = 2), "not identical")
    expect_warning(dotproduct(x, y, n = 2), "not identical")
    expect_equal(dotproduct(x, y, m = 0.5, n = 0), 0.4280249, tolerance = 1e-06)
    expect_equal(dotproduct(x, y, m = 0, n = 0), 1)
    expect_equal(dotproduct(x, y, n = 0), 0.4280249, tolerance = 1e-06)
    
    
    ## if the identical spectra are passed, dotproduct has to return 1
    expect_equal(dotproduct(x, x, n = 2), 1)
    expect_equal(dotproduct(y, y, n = 2), 1)
    
    ## check exceptions
    expect_error(dotproduct(x[-1, ], y), "nrow[(]x[)] and nrow[(]y[)]")
    expect_error(dotproduct(x, y[-1, ]), "nrow[(]x[)] and nrow[(]y[)]")
    expect_error(dotproduct(x = x), "is missing, with no default")
    expect_error(dotproduct(y = y), "is missing, with no default")
    expect_error(dotproduct(x = x, y = "a"), "not a matrix")
    expect_error(dotproduct(x = "a", y = y), "not a matrix")
    expect_error(dotproduct(x, y, m = "a", n = 0), 
                 "has to be a numeric of length 1")
    expect_error(dotproduct(x, y, m = 0.5, n = "a"), 
                 "has to be a numeric of length 1")
    expect_error(dotproduct(x, y, m = 1:2, n = 0),
                 "has to be a numeric of length 1")
    expect_error(dotproduct(x, y, m = 0.5, n = 1:2),
                 "has to be a numeric of length 1")
    
    ## check for columns 'mz' and 'intensity'
    x <- matrix(c(1:3, 1:3), ncol = 2)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- c("foo", "intensity")
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y), "subscript out of bounds")
    colnames(x) <- c("mz", "foo")
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y), "subscript out of bounds")
    colnames(x) <- c("mz", "intensity")
    colnames(y) <- c("foo", "intensity")
    expect_error(dotproduct(x, y), "subscript out of bounds")
    colnames(x) <- c("mz", "intensity")
    colnames(y) <- c("mz", "foo")
    expect_error(dotproduct(x, y), "subscript out of bounds")
    
    ## test for matrix
    x <- list(mz = 1:3, intensity = 1:3)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y), "is not a matrix")
    
    x <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- c("mz", "intensity")
    y <- list(mz = 1:3, intensity = 1:3)
    expect_error(dotproduct(x, y), "is not a matrix")
    
    ## test for mode 'numeric'
    x <- matrix(c(c("a", "b", "c"), c(1:3)), ncol = 2)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y), "non-numeric argument to binary operator")
    x <- matrix(c(1:3, 1:3), ncol = 2) 
    y <- matrix(c(c("a", "b", "c"), c(1:3)), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y), "non-numeric argument to binary operator")
    
})