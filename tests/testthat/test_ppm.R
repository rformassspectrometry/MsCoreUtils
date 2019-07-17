test_that("ppm", {
    expect_error(ppm("foo"))
    expect_warning(ppm(1:2, 3:5), "longer object")
    expect_equal(ppm(c(1000, 2000), 5), c(0.005, 0.01))
    expect_equal(ppm(c(1000, 2000), c(5, 10)), c(0.005, 0.02))
})
