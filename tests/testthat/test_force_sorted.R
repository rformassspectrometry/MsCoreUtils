test_that("forceSorting works", {
    vec <- c(NA, NA, NA, 1.2, 1.1, 1.14, 1.2, 1.3, 1.1, 1.04, 1.4, 1.6, NA, NA)
    # Expected result after interpolation
    sorted <- c(NA, NA, NA, 1.2, 1.225, 1.25, 1.275, 1.3, 1.333, 1.367,
                1.4, 1.6, NA, NA)
    result <- force_sorted(vec)
    expect_equal(result, sorted, tolerance = 0.001)

    # Test with decreasing values at the end
    vec <- c(NA, NA, NA, 1.2, 1.1, 1.14, 1.2, 1.3, 1.4, 1.04, 1.2, 1.04, NA)
    expect_warning(result <- force_sorted(vec), "Replacing")
    sorted <- c(NA, NA, NA, 1.2, 1.225, 1.25, 1.275, 1.3, 1.4, 1.400001,
                1.400002, 1.400003, NA)
    expect_equal(result, sorted)

    # Test with sorted values
    vec <- c(NA, NA, NA, 1.2, 1.3, 1.42, 1.46, 1.49, 1.498, 1.5, 1.6, 1.66, NA)
    result <- force_sorted(vec)
    expect_equal(vec, result)
})
