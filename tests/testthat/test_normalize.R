test_that("function: all normalize methods", {
    for (.method in normalizeMethods()) {
        x_norm <- normalize_matrix(x, method = .method)
        expect_identical(dim(x_norm), dim(x))
    }
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        for (.method in normalizeMethods()) {
            x_norm <- normalize_matrix(x, method = .method)
            x_normhdf5 <- normalize_matrix(xhdf5, method = .method)
            expect_true(inherits(x_normhdf5, "HDF5Matrix"))
            expect_identical(as.matrix(x_normhdf5), x_norm)
        }  
        ## Remove file
        unlink(tmpf)
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
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        mhdf5 <- HDF5Array::writeHDF5Array(m, tmpf, with.dimnames = TRUE)
        m_cmn_hdf5 <- normalize_matrix(mhdf5, method = "center.mean")
        expect_true(inherits(m_cmn_hdf5, "HDF5Matrix"))
        expect_identical(as.matrix(m_cmn_hdf5), m_cmn)
        m_cmd_hdf5 <- normalize_matrix(mhdf5, method = "center.median")
        expect_true(inherits(m_cmn_hdf5, "HDF5Matrix"))
        expect_identical(as.matrix(m_cmd_hdf5), m_cmd)
        ## Remove file
        unlink(tmpf)
    }
})

test_that("function: div.mean and div.median", {
    m <- matrix(rnorm(40, 10), ncol = 4)
    m_dmn <- normalize_matrix(m, method = "div.mean")
    m_dmd <- normalize_matrix(m, method = "div.median")
    expect_equal(colMeans(m_dmn), rep(1, 4))
    expect_equal(apply(m_dmd, 2, median), rep(1, 4))
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        mhdf5 <- HDF5Array::writeHDF5Array(m, tmpf, with.dimnames = TRUE)
        m_dmn_hdf5 <- normalize_matrix(mhdf5, method = "div.mean")
        expect_true(inherits(m_dmn_hdf5, "HDF5Matrix"))
        expect_identical(as.matrix(m_dmn_hdf5), m_dmn)
        m_dmd_hdf5 <- normalize_matrix(mhdf5, method = "div.median")
        expect_true(inherits(m_dmd_hdf5, "HDF5Matrix"))
        expect_identical(as.matrix(m_dmd_hdf5), m_dmd)
        ## Remove file
        unlink(tmpf)
    }
})


test_that("function: diff.median", {
    m <- matrix(rnorm(40, 10), ncol = 4)
    m_dmed <- normalize_matrix(m, method = "diff.median")
    expect_equal(apply(m_dmed, 2, median),
                 rep(median(m), 4))
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        mhdf5 <- HDF5Array::writeHDF5Array(m, tmpf, with.dimnames = TRUE)
        m_dmed_hdf5 <- normalize_matrix(mhdf5, method = "diff.median")
        expect_true(inherits(m_dmed_hdf5, "HDF5Matrix"))
        expect_identical(as.matrix(m_dmed_hdf5), m_dmed)
        ## Remove file
        unlink(tmpf)
    }
})
