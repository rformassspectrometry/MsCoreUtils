x <- matrix(c(1:5, 1:5), ncol = 2)
y <- matrix(c(1:5, 5:1), ncol = 2)

test_that("ndotproduct", {
    expect_equal(ndotproduct(x, x), 1)
    expect_equal(ndotproduct(x, y), 0.7661, tolerance = 1e-5)
    expect_equal(ndotproduct(x, y, m = 3, n = 0.6), 0.91276, tolerance = 1e-5)
})

test_that(".calibrate", {
    expect_equal(.calibrate(1:3), 1:3)
    expect_equal(.calibrate(1:3, 2, 2), (-1:1)/2)
    expect_equal(.calibrate(1:3, scaling = 0), rep(Inf, 3))
})

test_that(".weightxy", {
    expect_equal(.weightxy(1:3, 4:6), sqrt(4:6))
    expect_equal(.weightxy(1:3, 4:6, m = -1, n = 2), (4:6)*(4:6)/1:3)
})
