test_that("dotproduct", {
    
    ## create two dummy spectra (aligned)
    x <- data.frame(mz=c(100.002, 100.001, NA, 300.01, 300.02, NA), 
             intensity=c(2, 1.5, 0, 1.2, 0.9, 0))
    y <- data.frame(mz=c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
             intensity=c(2, 0, 3, 1, 4, 0.4))
    
    ## calculate similarity values
    expect_warning(dotproduct(x, y, m=0, n=2))
    expect_warning(dotproduct(x, y, n=2))
    expect_equal(dotproduct(x, y, m=0.5, n=0), 0.4280249, tolerance=1e-06)
    expect_equal(dotproduct(x, y, m=0, n=0), 1)
    expect_equal(dotproduct(x, y, n=0), 0.4280249, tolerance=1e-06)
    
    
    ## if the identical spectra are passed, dotproduct has to return 1
    expect_equal(dotproduct(x, x, n=2), 1)
    expect_equal(dotproduct(y, y, n=2), 1)
    
    ## check exceptions
    expect_error(dotproduct(x[-1, ], y))
    expect_error(dotproduct(x, y[-1, ]))
    expect_error(dotproduct(x=x))
    expect_error(dotproduct(y=y))
    expect_error(dotproduct(x=x, y="a"))
    expect_error(dotproduct(x="a", y=y))
    
    x <- list(mz = 1:3, intensity = 1:3)
    y <- list(mz = 1:3, intensity = 1:10)
    expect_error(dotproduct(x, y))
})