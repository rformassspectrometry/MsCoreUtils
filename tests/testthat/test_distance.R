test_that(".calibrate", {
    expect_equal(.calibrate(1:3), 1:3)
    expect_equal(.calibrate(1:3, 2, 2), (-1:1)/2)
    expect_equal(.calibrate(1:3, scaling = 0), rep(Inf, 3))
})
