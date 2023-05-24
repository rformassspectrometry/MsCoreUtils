test_that("entropy, nentropy works", {
    x <- c(37.16, 66.83, 999)
    res <- entropy(x)
    ## check result with reference from publication
    expect_equal(res, 0.3737888038158417)

    expect_equal(entropy(numeric()), 0)

    expect_equal(entropy(c(12.3, NA, 23)), NA_real_)

    res <- nentropy(x)
    expect_equal(res, entropy(x) / log(3))

    expect_equal(nentropy(numeric()), 0)
})

