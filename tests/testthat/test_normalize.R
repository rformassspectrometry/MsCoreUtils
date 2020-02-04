test_that("function: all normalize methods", {
    for (.method in normalizeMethods()) {
        x_norm <- normalize_matrix(x, method = .method)
        expect_identical(dim(x_norm), dim(x))
    }
})

test_that("function: center.mean and center.median", {
    m <- matrix(1:40, ncol = 4)
    m_cmn <- normalize_matrix(m, method = "center.mean")
    ## col means are mean(1:10) == 5.5 and 15.5, ..., 35.5
    m_exp <- m
    cm <- (5.5 + seq(0, 30, 10))
    for (i in 1:4)
        m_exp[, i] <- m_exp[, i] - cm[i]
    expect_identical(m_cmn, m_exp)
    ## col medians are the same
    expect_identical(apply(m, 2, median), colMeans(m))
    m_cmd <- normalize_matrix(m, method = "center.median")
    expect_identical(m_cmn, m_cmd)
})

test_that("function: div.mean and div.median", {
    m <- matrix(rnorm(40, 10), ncol = 4)
    m_dmn <- normalize_matrix(m, method = "div.mean")
    m_dmd <- normalize_matrix(m, method = "div.median")
    expect_equal(colMeans(m_dmn), rep(1, 4))
    expect_equal(apply(m_dmd, 2, median), rep(1, 4))
})


test_that("function: diff.median", {
    m <- matrix(rnorm(40, 10), ncol = 4)
    m_dmed <- normalize_matrix(m, method = "diff.median")
    expect_equal(apply(m_dmed, 2, median),
                 rep(median(m), 4))
})
