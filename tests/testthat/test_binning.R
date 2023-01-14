test_that(".fix_breaks works", {
    rtr <- c(1, 12)
    brks <- seq(rtr[1], rtr[2], by = 4)
    ## brks do not include rtr
    expect_true(all(rtr[2] > brks))
    brks_f <- .fix_breaks(brks, rtr)
    expect_true(rtr[2] <= max(brks_f))

    ## Next unit tests taken from breaks_Spectrum.
    ## Issue #191
    ints <- 1:4
    brks <- seq(min(ints), max(ints), by = 1)
    expect_equal(.fix_breaks(brks, range(ints)), 1:5)
    expect_true(any(.fix_breaks(1:2, range(ints)) < 4))
    expect_equal(.fix_breaks(seq(1, 4, by = 2), range(ints)),
                 c(1, 3, 5))

    ## Test with values smaller than 1
    rng <- c(0.1, 0.4)
    brks <- seq(rng[1], rng[2], by = 0.04)
    .fix_breaks(brks, rng)
})

test_that("bin works", {
    set.seed(123)
    vals <- abs(rnorm(20, mean = 40))
    xs <- seq(1:length(vals)) + rnorm(length(vals), sd = 0.001)

    res <- bin(vals, xs, size = 1)
    brks <- seq(0, 20, by = 1)
    for (i in 1:(length(brks) - 1)) {
        idx <- which(xs > brks[i] & xs < brks[i +1])
        if (length(idx))
            expect_equal(res$x[i], max(vals[idx]))
        else expect_equal(res$x[i], 0)
    }

    ## Ensure that all values are within.
    xs <- seq(1:length(vals))
    brks <- seq(1, 20, by = 3)
    ## brks does not contain all values.
    expect_true(max(brks) < max(xs))
    res <- bin(vals, xs, size = 3, FUN = sum)
    ## The largest bin should contain all values larger than max(brks)
    expect_equal(res$x[length(res$x)], sum(vals[xs >= max(brks)]))
    ## without reporting mid points.
    res_2 <- bin(vals, xs, size = 3, FUN = sum, returnMids = FALSE)
    expect_true(is.numeric(res_2))
    expect_equal(res$x, res_2)

    ## Ensure y order does not matter
    ## Issue #108 https://github.com/rformassspectrometry/MsCoreUtils/issues/108
    expect_equal(bin(vals, xs), bin(rev(vals), rev(xs)))
    xs_unsorted <- sample(xs, length(xs))
    expect_equal(bin(vals, xs_unsorted),
                 bin(vals[order(xs_unsorted)], sort(xs_unsorted)))
    

    ## Check exceptions
    expect_error(bin(1:3, 1:5))
    expect_error(bin(1:3, 1:5), FUN = other)
})
