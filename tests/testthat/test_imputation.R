test_that("all imputation methods", {
    m <- imputeMethods()
    m <- m[m != "mixed"]
    m <- m[m != "none"]
    m <- m[m != "with"]  ## see below
    m <- m[m != "nbavg"] ## see next test
    for (.m in m) {
        xx <- impute_matrix(x, method = .m)
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
})

test_that("none method", {
    xx <- impute_matrix(x, method = "none")
    expect_identical(x, xx)
})

test_that("zero and with method", {
    x1 <- impute_matrix(x, method = "with", val = 0)
    x2 <- impute_matrix(x, method = "zero")
    expect_identical(x1, x2)
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
    x_imp <- impute_matrix(x, method = "knn")
    x_imp_2 <- impute_matrix(x_imp, method = "knn")
    expect_identical(x_imp, x_imp_2)
})
