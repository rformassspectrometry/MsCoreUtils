test_that("group works", {
    set.seed(123)
    ## Simulate m/z values for identical ions from 3 spectra, differing only
    ## by a small margin.
    x <- seq(1, 20, 0.1)
    all_mz <- sort(c(x + rnorm(length(x), sd = 0.001),
                     x + rnorm(length(x), sd = 0.005),
                     x + rnorm(length(x), sd = 0.002)))
    res <- group(all_mz)
    expect_true(length(res) == length(all_mz))
    expect_equal(res, 1:length(all_mz))

    res <- group(all_mz, tolerance = 0.05)
    ## Expect groups of 3 each.
    expect_true(all(table(res) == 3))

    ## randomly shuffle
    idx <- sample(1:length(all_mz), size = length(all_mz))
    res <- group(all_mz[idx], tolerance = 0.05)
    expect_true(all(table(res) == 3))
    expect_equal(res[order(idx)], rep(1:(length(all_mz)/3), each = 3))

    ## Remove one from the 2nd group.
    res <- group(all_mz[-5], tolerance = 0.05)
    expect_true(sum(res == 2) == 2)

    ## With ppm
    x <- c(34, 56, 56 + ppm(56, 10), 66)
    res <- group(x, ppm = 10)
    expect_equal(res, c(1L, 2L, 2L, 3L))

    x <- c(56 + ppm(56, 10), 34, 56, 66)
    res <- group(x, ppm = 10)
    expect_equal(res, c(2L, 1L, 2L, 3L))

    ## Manual tolerance
    x <- c(34, 56, 56 + ppm(56, 10) + 0.1, 66, 66.05)
    res <- group(x, tolerance = 0.05)
    expect_equal(res, c(1L, 2L, 3L, 4L, 4L))

    res <- group(x, tolerance = 0.05, ppm = 10)
    expect_equal(res, c(1L, 2L, 3L, 4L, 4L))

    res <- group(x, tolerance = 0.1, ppm = 10)
    expect_equal(res, c(1L, 2L, 2L, 3L, 3L))

    x <- c(34, 56, 66, 56.1, 56.05, 66 + ppm(66, 10), 34.1)
    res <- group(x, tolerance = 0.1)
    expect_equal(res, c(1L, 2L, 3L, 2L, 2L, 3L, 1L))
})
