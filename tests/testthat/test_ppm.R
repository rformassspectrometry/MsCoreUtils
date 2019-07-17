test_that("ppm", {
    expect_error(ppm("foo"))
    expect_equal(ppm(5), 5 * 1e-6)
})
