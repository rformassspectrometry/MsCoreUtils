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


test_that("impute_mixed(MARGIN = c(1,2), marArgs, mnarArgs) works (1)", {
    m <- matrix(1:18, ncol = 3)
    m[c(2, 4), 2] <- m[1, 3] <- m[c(3,5), 1] <- m[6, 3] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 3))
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(1, 2),
                          marArgs = list(q = 0.01), ## default value
                          mnarArgs = list(q = 0))   ## use min value
    ###########################################################
    ## Expected: m[1:3, ] MNAR, MARGIN 2
    ##       +------------- m[3, 1] is quantile(m[1:3, 1], 0)
    ##       |     +------- m[2, 2] is quantile(m[1:3, 2], 0)
    ##       |     |    +-- m[1, 3] is quantile(m[1:3, 3], 0)
    ##       |     |    |
    ##       V     V    V
    ##      [,1] [,2] [,3]
    ## [1,]    1    7   NA
    ## [2,]    2   NA   14
    ## [3,]   NA    9   15
    expect_identical(m_imp[1, 3],
                     quantile(m[1:3, 3], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[1:3, 2], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[3, 1],
                     quantile(m[1:3, 1], 0, na.rm = TRUE,
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

test_that("impute_mixed(MARGIN = c(1,2), marArgs, mnarArgs) works (2)", {
    m <- matrix(1:18, ncol = 3)
    m[c(2, 4), 2] <- m[1, 3] <- m[c(3,5), 1] <- m[6, 3] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 3))
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(1, 2),
                          marArgs = list(q = 0),     ## use min value
                          mnarArgs = list(q = 0.01)) ## default value
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
    ## [4,]    4   NA   16 <--- m[4, 2] is quantile(m[4, ], 0)
    ## [5,]   NA   11   17 <--- m[5, 1] is quantile(m[5, ], 0)
    ## [6,]    6   12   NA <--- m[6, 3] is quantile(m[6, ], 0)
    expect_identical(m_imp[4, 2],
                     quantile(m[4, ], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[5, 1],
                     quantile(m[5, ], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[6, 3],
                     quantile(m[6, ], 0, na.rm = TRUE,
                              names = FALSE))
})


test_that("impute_mixed(MARGIN = c(1,2), marArgs, mnarArgs) works (3)", {
    m <- matrix(1:18, ncol = 3)
    m[c(2, 4), 2] <- m[1, 3] <- m[c(3,5), 1] <- m[6, 3] <- NA
    randna <- c(rep(FALSE, 3), rep(TRUE, 3))
    m_imp <- impute_mixed(m, randna = randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(1, 2),
                          marArgs = list(q = 0),  ## use min value
                          mnarArgs = list(q = 1)) ## use max value
    ###########################################################
    ## Expected: m[1:3, ] MNAR, MARGIN 2
    ##       +------------- m[3, 1] is quantile(m[1:3, 1], 1)
    ##       |     +------- m[2, 2] is quantile(m[1:3, 2], 1)
    ##       |     |    +-- m[1, 3] is quantile(m[1:3, 3], 1)
    ##       |     |    |
    ##       V     V    V
    ##      [,1] [,2] [,3]
    ## [1,]    1    7   NA
    ## [2,]    2   NA   14
    ## [3,]   NA    9   15
    expect_identical(m_imp[1, 3],
                     quantile(m[1:3, 3], 1, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[2, 2],
                     quantile(m[1:3, 2], 1, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[3, 1],
                     quantile(m[1:3, 1], 1, na.rm = TRUE,
                              names = FALSE))
    ###########################################################
    ## Expected: m[4:6, ] MAR, MARGIN 1
    ##      [,1] [,2] [,3]
    ## [4,]    4   NA   16 <--- m[4, 2] is quantile(m[4, ], 0)
    ## [5,]   NA   11   17 <--- m[5, 1] is quantile(m[5, ], 0)
    ## [6,]    6   12   NA <--- m[6, 3] is quantile(m[6, ], 0)
    expect_identical(m_imp[4, 2],
                     quantile(m[4, ], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[5, 1],
                     quantile(m[5, ], 0, na.rm = TRUE,
                              names = FALSE))
    expect_identical(m_imp[6, 3],
                     quantile(m[6, ], 0, na.rm = TRUE,
                              names = FALSE))
})

test_that("impute_mixed(MARGIN = 1, split) works", {
    ## When MARGIN = 1, the splitting does not change the results
    set.seed(123)
    m <- matrix(rnorm(50), nrow = 10)
    diag(m) <- NA
    randna <- rep(c(TRUE, FALSE), each = 5)
    mimp1 <- impute_mixed(m, randna,
                          mar = "MinDet",
                          mnar = "knn",
                          MARGIN = c(1L, 1L),
                          split = TRUE) ## DEFAULT
    mimp2 <- impute_mixed(m, randna,
                          mar = "MinDet",
                          mnar = "knn",
                          MARGIN = c(1L, 1L),
                          split = FALSE)
    expect_identical(mimp1, mimp2)
})

test_that("impute_mixed(MARGIN = 2, split) works", {
    ## When MARGIN = 2, the splitting does change the results.
    m <- matrix(1:32, nrow = 8)
    diag(m) <- NA
    randna <- rep(c(TRUE, FALSE), each = 4)
    ###########################################################
    ## SPLITTING -- only consider the MAR sub-matrix
    ##
    ##       +---- quantile(m[1:4, i], 0.01)
    ##       |
    ##       V
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   NA    9   17   25
    ## [2,]    2   NA   18   26
    ## [3,]    3   11   NA   27
    ## [4,]    4   12   20   NA
    mimp <- impute_mixed(m, randna,
                         mar = "MinDet",
                         mnar = "zero",
                         MARGIN = c(2L, 2L),
                         split = TRUE)
    expect_identical(
        mimp[1, 1],
        quantile(m[1:4, 1], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[2, 2],
        quantile(m[1:4, 2], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[3, 3],
        quantile(m[1:4, 3], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[4, 4],
        quantile(m[1:4, 4], 0.01, na.rm = TRUE, names = FALSE))
    ###########################################################
    ## NOT SPLITTING -- consider the full matrix
    ##       +---- quantile(m[, i], 0.01)
    ##       |
    ##       V
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   NA    9   17   25
    ## [2,]    2   NA   18   26
    ## [3,]    3   11   NA   27
    ## [4,]    4   12   20   NA
    ## [5,]    5   13   21   29
    ## [6,]    6   14   22   30
    ## [7,]    7   15   23   31
    ## [8,]    8   16   24   32
    mimp <- impute_mixed(m, randna,
                         mar = "MinDet",
                         mnar = "zero",
                         MARGIN = c(2L, 2L),
                         split = FALSE)
    expect_identical(
        mimp[1, 1],
        quantile(m[, 1], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[2, 2],
        quantile(m[, 2], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[3, 3],
        quantile(m[, 3], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp[4, 4],
        quantile(m[, 4], 0.01, na.rm = TRUE, names = FALSE))
})

test_that("impute_mixed(MARGIN = c(2, 1), split) works", {
    ## When MARGIN = 2, the splitting does change the results.
    ## We keep MARGIN = 1 for MNAR (using MinDet).
    m <- matrix(1:32, nrow = 8)
    diag(m) <- NA
    m[5, 1] <- m[6, 2] <- m[7, 3] <- m[8, 4] <- NA
    randna <- rep(c(TRUE, FALSE), each = 4)
    ###########################################################
    ## SPLITTING -- only consider the MAR sub-matrix
    ##
    ##       +---- quantile(m[1:4, i], 0.01)
    ##       |
    ##       V
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   NA    9   17   25
    ## [2,]    2   NA   18   26
    ## [3,]    3   11   NA   27
    ## [4,]    4   12   20   NA
    ## [5,]   NA   13   21   29 <- quantile(m[i, ], 0.01)
    ## [6,]    6   NA   22   30 <- quantile(m[i, ], 0.01)
    ## [7,]    7   15   NA   31 <- quantile(m[i, ], 0.01)
    ## [8,]    8   16   24   NA <- quantile(m[i, ], 0.01)
    mimp1 <- impute_mixed(m, randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(2L, 1L),
                          split = TRUE)
    ## As in previous test "impute_mixed(MARGIN = 2, split) works"
    expect_identical(
        mimp1[1, 1],
        quantile(m[1:4, 1], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp1[2, 2],
        quantile(m[1:4, 2], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp1[3, 3],
        quantile(m[1:4, 3], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp1[4, 4],
        quantile(m[1:4, 4], 0.01, na.rm = TRUE, names = FALSE))
    ###########################################################
    ## NOT SPLITTING -- consider the full matrix
    ##       +---- quantile(m[, i], 0.01)
    ##       |
    ##       V
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   NA    9   17   25
    ## [2,]    2   NA   18   26
    ## [3,]    3   11   NA   27
    ## [4,]    4   12   20   NA
    ## [5,]   NA   13   21   29 <- quantile(m[i, ], 0.01)
    ## [6,]    6   NA   22   30 <- quantile(m[i, ], 0.01)
    ## [7,]    7   15   NA   31 <- quantile(m[i, ], 0.01)
    ## [8,]    8   16   24   NA <- quantile(m[i, ], 0.01)
    mimp2 <- impute_mixed(m, randna,
                          mar = "MinDet",
                          mnar = "MinDet",
                          MARGIN = c(2L, 1L),
                          split = FALSE)
    ## As in previous test "impute_mixed(MARGIN = 2, split) works"
    expect_identical(
        mimp2[1, 1],
        quantile(m[, 1], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[2, 2],
        quantile(m[, 2], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[3, 3],
        quantile(m[, 3], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[4, 4],
        quantile(m[, 4], 0.01, na.rm = TRUE, names = FALSE))
    ###########################################################
    ## Bottom MNAR sub-matrices, impute with MinDet (MARGIN = 1) are identical
    ##
    ## [5,]   NA   13   21   29 <- quantile(m[i, ], 0.01)
    ## [6,]    6   NA   22   30 <- quantile(m[i, ], 0.01)
    ## [7,]    7   15   NA   31 <- quantile(m[i, ], 0.01)
    ## [8,]    8   16   24   NA <- quantile(m[i, ], 0.01)
    expect_identical(mimp1[5:8, ], mimp2[5:8, ])
    expect_identical(
        mimp2[5, 1],
        quantile(m[5, ], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[6, 2],
        quantile(m[6, ], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[7, 3],
        quantile(m[7, ], 0.01, na.rm = TRUE, names = FALSE))
    expect_identical(
        mimp2[8, 4],
        quantile(m[8, ], 0.01, na.rm = TRUE, names = FALSE))
})
