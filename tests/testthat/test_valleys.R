test_that("valleys", {
    expect_error(valleys(TRUE, 1), "numeric")
    expect_error(valleys(1:10, TRUE), "integer")

    expect_equal(valleys(c(1:5, 4:1), 5),
                 matrix(c(1, 5, 9), nrow = 1,
                        dimnames = list(c(), c("left", "center", "right"))))
    ## known bug/problem with plateaus c(0, 0) because localMaxima returns just
    ## the first occurence of a local maximum
    expect_equal(valleys(c(1:5, 4:1, 0, 0:3, 2), c(5, 14)),
                 matrix(c(1, 5, 10, 10, 14, 15), nrow = 2, byrow = TRUE,
                        dimnames = list(c(), c("left", "center", "right"))))
})
