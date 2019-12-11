test_that(".calibrate", {
    expect_equal(.calibrate(1:3), 1:3)
    expect_equal(.calibrate(1:3, 2, 2), (-1:1)/2)
    expect_equal(.calibrate(1:3, scaling = 0), rep(Inf, 3))
})

test_that(".weightxy", {
    expect_equal(.weightxy(1:3, 4:6), sqrt(4:6))
    expect_equal(.weightxy(1:3, 4:6, m = -1, n = 2), (4:6)*(4:6)/1:3)
})
