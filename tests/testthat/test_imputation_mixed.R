test_that("impute_mixed(margin) works (1)", {
    m <- matrix(1:12, ncol = 4)
    m[2:3, 2] <- m[3, 3] <- NA
    rand <- c(TRUE, TRUE, FALSE)
    ## MARGIN 1
    m_imp <- impute_mixed(m, randna = rand,
                          mar = "MinDet",
                          mnar = "zero",
                          MARGIN = 1L)
    expect_identical(m_imp[2, 2],
                     quantile(m[2, ], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(m_imp[3, 2:3], c(0, 0))
    ## MARGIN 2
    m_imp <- impute_mixed(m, randna = rand,
                          mar = "MinDet",
                          mnar = "zero",
                          MARGIN = 2L)
    expect_identical(m_imp[2, 2],
                     quantile(m[, 2], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(m_imp[3, 2:3], c(0, 0))
})

test_that("impute_mixed(margin) works (2)", {
    m <- matrix(1:15, ncol = 3)
    m[2:4, 2] <- m[1, 3] <- m[5, 1] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 2))
    ## MARGIN 1
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "zero",
                          mnar = "MinDet",
                          MARGIN = 1)
    ## Expected:
    ## - m[1, 3] is quantile(m[1, ], 0.01, na.rm = TRUE)
    ## - m[2, 2] is quantile(m[2, ], 0.01, na.rm = TRUE)
    ## - m[3, 2] is quantile(m[3, ], 0.01, na.rm = TRUE)
    ## - m[4, 2] is zero
    ## - m[5, 1] is zero
    expect_identical(m_imp[1, 3],
                     quantile(m[1, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[2, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[2, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[4, 2], 0)
    expect_identical(m_imp[5, 1], 0)
    ## MARGIN 2
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "zero",
                          mnar = "MinDet",
                          MARGIN = 2)
    ## Expected:
    ## - m[1, 3] is quantile(m[1:3, 3], 0.01, na.rm = TRUE)
    ## - m[2:3, 2] is quantile(m[1:3, 2], 0.01, na.rm = TRUE)
    ## - m[4, 2] is zero
    ## - m[5, 1] is zero
    expect_identical(m_imp[1, 3],
                     quantile(m[1:3, 3], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(m_imp[2:3, 2],
                     c(quantile(m[1:3, 2], 0.01, na.rm = TRUE, names = FALSE),
                       quantile(m[1:3, 2], 0.01, na.rm = TRUE, names = FALSE)))
    expect_identical(m_imp[4, 2], 0)
    expect_identical(m_imp[5, 1], 0)
})

test_that("impute_mixed(MARGIN=integer(1) or integer(2)) works (3)", {
    m <- matrix(1:15, ncol = 3)
    m[2:4, 2] <- m[1, 3] <- m[5, 1] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 2))
    ## MARGIN 1 - same example data as in (2)
    m_imp1 <- impute_mixed(m, randna = randna,
                           mar = "zero",
                           mnar = "MinDet",
                           MARGIN = 1)
    m_imp2 <- impute_mixed(m, randna = randna,
                           mar = "zero",
                           mnar = "MinDet",
                           MARGIN = c(1, 1))
    expect_identical(m_imp1, m_imp2)
    ## MARGIN 2  - same example data as in (2)
    m_imp1 <- impute_mixed(m, randna = randna,
                           mar = "zero",
                           mnar = "MinDet",
                           MARGIN = 2)
    m_imp2 <- impute_mixed(m, randna = randna,
                          mar = "zero",
                          mnar = "MinDet",
                          MARGIN = c(2, 2))
    expect_identical(m_imp1, m_imp2)
})

test_that("impute_mixed(MARGIN = c(1,2)) works", {
    m <- matrix(1:18, ncol = 3)
    m[c(2, 4), 2] <- m[1, 3] <- m[c(3,5), 1] <- m[6, 3] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 3))
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(1, 2))
    ###########################################################
    ## Expected: m[1:3, ] MNAR, MARGIN 2
    ##       +------------- m[3, 1] is quantile(m[1:3, 1], 0.01)
    ##       |     +------- m[2, 2] is quantile(m[1:3, 2], 0.01)
    ##       |     |    +-- m[1, 3] is quantile(m[1:3, 3], 0.01)
    ##       |     |    |
    ##       V     V    V
    ##      [,1] [,2] [,3]
    ## [1,]    1    7   NA
    ## [2,]    2   NA   14
    ## [3,]   NA    9   15
    expect_identical(m_imp[1, 3],
                     quantile(m[1:3, 3], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[1:3, 2], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[3, 1],
                     quantile(m[1:3, 1], 0.01, na.rm = TRUE,
                              names = FALSE))
    ###########################################################
    ## Expected: m[4:6, ] MAR, MARGIN 1
    ##      [,1] [,2] [,3]
    ## [4,]    4   NA   16 <--- m[4, 2] is quantile(m[4, ], 0.01)
    ## [5,]   NA   11   17 <--- m[5, 1] is quantile(m[5, ], 0.01)
    ## [6,]    6   12   NA <--- m[6, 3] is quantile(m[6, ], 0.01)
    expect_identical(m_imp[4, 2],
                     quantile(m[4, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[5, 1],
                     quantile(m[5, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[6, 3],
                     quantile(m[6, ], 0.01, na.rm = TRUE,
                              names = FALSE))
})

test_that("impute_mixed(MARGIN = c(2,1)) works", {
    m <- matrix(1:18, ncol = 3)
    m[2:4, 2] <- m[1, 3] <- m[5, 1] <- m[6, 3] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 3))
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(2, 1))
    ###########################################################
    ## Expected: m[1:3, ] MNAR, MARGIN 1
    ##      [,1] [,2] [,3]
    ## [1,]    1    7   NA <--- m[1, 3] is quantile(m[1, ], 0.01)
    ## [2,]    2   NA   14 <--- m[2, 2] is quantile(m[2, ], 0.01)
    ## [3,]    3   NA   15 <--- m[3, 2] is quantile(m[3, ], 0.01)
    expect_identical(m_imp[1, 3],
                     quantile(m[1, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[2, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[2, ], 0.01, na.rm = TRUE,
                              names = FALSE))
    ###########################################################
    ## Expected: m[4:6, ] MAR, MARGIN 2
    ##       +------------- m[5, 1] is quantile(m[4:6, 1], 0.01)
    ##       |     +------- m[4, 2] is quantile(m[4:6, 2], 0.01)
    ##       |     |    +-- m[6, 3] is quantile(m[4:6, 3], 0.01)
    ##       |     |    |
    ##       V     V    V
    ##      [,1] [,2] [,3]
    ## [4,]    4   NA   16
    ## [5,]   NA   11   17
    ## [6,]    6   12   NA
    expect_identical(m_imp[4, 2],
                     quantile(m[4:6, 2], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[5, 1],
                     quantile(m[4:6, 1], 0.01, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[6, 3],
                     quantile(m[4:6, 3], 0.01, na.rm = TRUE,
                              names = FALSE))
})
