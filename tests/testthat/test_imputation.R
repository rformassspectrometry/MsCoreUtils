test_that("all imputation methods", {
    m <- imputeMethods()
    m <- m[m != "mixed"]
    m <- m[m != "none"]
    m <- m[m != "with"]  ## see below
    m <- m[m != "nbavg"] ## see next test
    for (.m in m) {
        if (.m == "knn") {
            expect_warning(xx <- impute_matrix(x, method = .m),
                           regexp = "more than.*entries missing")
        } else {
            xx <- impute_matrix(x, method = .m)
        }
        expect_false(any(is.na(xx)))
    }
    expect_error(impute_matrix(x, method = "mixed",
                               randna = randna,
                               mnar = "min"),
                 regexp = "mar")
    expect_error(impute_matrix(x, method = "mixed",
                               randna = randna,
                               mar = "knn"),
                 regexp = "mnar")
    expect_error(impute_matrix(x, method = "mixed",
                               mnar = "min",
                               mar = "knn"),
                 regexp = "randna")
    expect_error(impute_matrix(x, method = "mixed",
                               randna = TRUE,
                               mnar = "min",
                               mar = "knn"),
                 regexp = "randna")
    mx <- impute_matrix(x, method = "mixed",
                        randna = randna,
                        mnar = "min",
                        mar = "knn")
    expect_false(any(is.na(mx)))
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        for (.m in m) {
            set.seed(1234)
            if (.m == "knn") {
                expect_warning(xxhdf5 <- impute_matrix(xhdf5, method = .m),
                               regexp = "more than.*entries missing")
            } else {
                xxhdf5 <- impute_matrix(xhdf5, method = .m)
            }
            expect_true(inherits(xxhdf5, "HDF5Matrix"))
            if (.m != "MLE") { ## MLE is non-deterministic (apparently)
                set.seed(1234)
                if (.m == "knn") {
                    expect_warning(xx <- impute_matrix(x, method = .m),
                                   regexp = "more than.*entries missing")
                } else {
                    xx <- impute_matrix(x, method = .m)
                }
                expect_equal(as.matrix(xxhdf5), xx)
            }
        }
        ## Remove file
        unlink(tmpf)
    }
})

test_that("none method", {
    xx <- impute_matrix(x, method = "none")
    expect_identical(x, xx)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        xxhdf5 <- impute_matrix(xhdf5, method = "none")
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), xx)
        ## Remove file
        unlink(tmpf)
    }
})

test_that("zero and with method", {
    x1 <- impute_matrix(x, method = "with", val = 0)
    x2 <- impute_matrix(x, method = "zero")
    expect_identical(x1, x2)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        xxhdf5 <- impute_matrix(xhdf5, method = "with", val = 0)
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), x1)
        xxhdf5 <- impute_matrix(xhdf5, method = "zero")
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), x2)
        ## Remove file
        unlink(tmpf)
    }
})

test_that("nbavg methods", {
    x2 <- matrix(1:25, 5)
    ## default min value
    x2[1, 2] <- 0.1
    ## imputes as min value (or use-defined k)
    x2[1, 1] <- x2[5, 5] <- NA
    x2[2, 1:2] <- NA ## [2, 1] will be min
                     ## [2, 2] will be avg 6.05
    ## remaing NA
    x2[3, 3:4] <- NA
    ## average imputation
    x2[5, 2] <- NA ## will be 10
    x2[4, 3] <- NA ## will be 14
    rownames(x2) <- colnames(x2) <-
            LETTERS[1:5]

    xx <- impute_matrix(x2, method = "nbavg")
    expect_true(xx[1, 2] == 0.1)
    expect_true(xx[1, 1] == 0.1)
    expect_true(xx[2, 1] == 0.1)
    expect_true(xx[2, 2] == 6.05)
    expect_true(all(is.na(xx[3, 3:4])))
    expect_true(xx[5, 2] == 10)
    expect_true(xx[4, 3] == 14)

    xx <- impute_matrix(x2, "nbavg", k = 0)
    expect_true(xx[1, 2] == 0.1)
    expect_true(xx[1, 1] == 0)
    expect_true(xx[2, 1] == 0)
    expect_true(xx[2, 2] == 6)
    expect_true(all(is.na(xx[3, 3:4])))
    expect_true(xx[5, 2] == 10)
    expect_true(xx[4, 3] == 14)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x2, tmpf, with.dimnames = TRUE)
        xxhdf5 <- impute_matrix(xhdf5, method = "nbavg", k = 0)
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), xx)
        ## Remove file
        unlink(tmpf)
    }
})


test_that("seed is not set by knn imputation method", {
  rand <- sapply(1:10, function(idx){
      xx <- suppressWarnings(impute_matrix(x, "knn"))
      rnorm(1)
  })
  expect_gt(max(rand) - min(rand), 0)
})

test_that("impute: mandatory method", {
    expect_error(impute_matrix(x))
    expect_error(impute_matrix(x, method = "not"))
})

test_that("impute: absence of missing values", {
    expect_warning(x_imp <- impute_matrix(x, method = "knn"),
                   regexp = "more than.*entries missing")
    x_imp_2 <- impute_matrix(x_imp, method = "knn")
    expect_identical(x_imp, x_imp_2)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x_imp, tmpf, with.dimnames = TRUE)
        xxhdf5 <- impute_matrix(xhdf5, method = "knn")
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), x_imp)
        ## Remove file
        unlink(tmpf)
    }
})

test_that("impute: user-provided function", {
    user_fun <- function(x, val = 1) {
        x[is.na(x)] <- val
        x
    }
    x3 <- x2 <- matrix(1:50, nrow = 10)
    x3[1, 1] <- NA
    ## default argument
    x_imp <- impute_matrix(x3, FUN = user_fun)
    x_imp2 <- user_fun(x3)
    expect_equal(x2, x_imp)
    expect_equal(x2, x_imp2)
    ## extra argument
    x_imp <- impute_matrix(x3, FUN = user_fun, val = 1000)
    x_imp2 <- user_fun(x3, val = 1000)
    x2[1, 1] <- 1000
    expect_equal(x2, x_imp)
    expect_equal(x2, x_imp2)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x3, tmpf, with.dimnames = TRUE)
        xxhdf5 <- impute_matrix(xhdf5, FUN = user_fun, val = 1000)
        expect_true(inherits(xxhdf5, "HDF5Matrix"))
        expect_equal(as.matrix(xxhdf5), x_imp)
        ## Remove file
        unlink(tmpf)
    }
})


test_that("impute_matrix() preserves dimnames", {
    ## imputation methods
    m <- imputeMethods()
    ## skip some - see 117 for MLE2
    m <- setdiff(imputeMethods(), c("MLE2", "mixed", "with"))
    ## test data
    set.seed(1)
    x_miss <- matrix(rnorm(100 * 10), ncol = 10)
    i <- sample(1:10, size = 3, replace = FALSE)
    j <- sample(1:10, size = 5, replace = FALSE)
    x_miss[i, j] <- NA_real_
    ## test with dimnames
    for (.m in m) {
        suppressWarnings(x_imp <- impute_matrix(x_miss, method = .m))
        expect_identical(dimnames(x_miss), dimnames(x_imp))
    }
    ## same, but with a matrix without dimnames
    x_miss2 <- x_miss
    dimnames(x_miss2) <- NULL
    for (.m in m) {
        suppressWarnings(x_imp <- impute_matrix(x_miss2, method = .m))
        expect_identical(dimnames(x_miss2), dimnames(x_imp))
    }
})
