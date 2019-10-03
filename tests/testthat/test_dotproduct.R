test_that("dotproduct", {
    
    ## create two dummy spectra (aligned)
    x <- matrix(c(c(100.002, 100.001, NA, 300.01, 300.02, NA), 
             c(2, 1.5, 0, 1.2, 0.9, 0)), ncol = 2)
    y <- matrix(c(c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
             intensity = c(2, 0, 3, 1, 4, 0.4)), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    
    ## calculate similarity values
    expect_warning(dotproduct(x, y, m = 0, n = 2))
    expect_warning(dotproduct(x, y, n = 2))
    expect_equal(dotproduct(x, y, m = 0.5, n = 0), 0.4280249, tolerance = 1e-06)
    expect_equal(dotproduct(x, y, m = 0, n = 0), 1)
    expect_equal(dotproduct(x, y, n = 0), 0.4280249, tolerance = 1e-06)
    
    
    ## if the identical spectra are passed, dotproduct has to return 1
    expect_equal(dotproduct(x, x, n = 2), 1)
    expect_equal(dotproduct(y, y, n = 2), 1)
    
    ## check exceptions
    expect_error(dotproduct(x[-1, ], y))
    expect_error(dotproduct(x, y[-1, ]))
    expect_error(dotproduct(x = x))
    expect_error(dotproduct(y = y))
    expect_error(dotproduct(x = x, y = "a"))
    expect_error(dotproduct(x = "a", y = y))
    expect_error(dotproduct(x, y, m = "a", n = 0))
    expect_error(dotproduct(x, y, m = 0.5, n = "a"))
    expect_error(dotproduct(x, y, m = 1:2, n = 0))
    expect_error(dotproduct(x, y, m = 0.5, n = 1:2))
    
    ## check for columns 'mz' and 'intensity'
    x <- matrix(c(1:3, 1:3), ncol = 2)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- c("foo", "intensity")
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y))
    colnames(x) <- c("mz", "foo")
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y))
    colnames(x) <- c("mz", "intensity")
    colnames(y) <- c("foo", "intensity")
    expect_error(dotproduct(x, y))
    colnames(x) <- c("mz", "intensity")
    colnames(y) <- c("mz", "foo")
    expect_error(dotproduct(x, y))
    
    ## test for matrix
    x <- list(mz = 1:3, intensity = 1:3)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y))
    
    x <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- c("mz", "intensity")
    y <- list(mz = 1:3, intensity = 1:3)
    expect_error(dotproduct(x, y))
    
    ## test for mode 'numeric'
    x <- matrix(c(c("a", "b", "c"), c(1:3)), ncol = 2)
    y <- matrix(c(1:3, 1:3), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y))
    x <- matrix(c(1:3, 1:3), ncol = 2) 
    y <- matrix(c(c("a", "b", "c"), c(1:3)), ncol = 2)
    colnames(x) <- colnames(y) <- c("mz", "intensity")
    expect_error(dotproduct(x, y))
    
})