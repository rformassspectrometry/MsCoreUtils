test_that(".checkMargin works", {
    expect_identical(MsCoreUtils:::.checkMargin(1L), 1L)
    expect_identical(MsCoreUtils:::.checkMargin(1), 1L)
    expect_identical(MsCoreUtils:::.checkMargin(2L), 2L)
    expect_identical(MsCoreUtils:::.checkMargin(2), 2L)
    expect_error(MsCoreUtils:::.checkMargin(3L))
    expect_error(MsCoreUtils:::.checkMargin(3))
    expect_error(MsCoreUtils:::.checkMargin(0))
})

test_that("test all default margins", {
    expect_identical(getImputeMargin(impute_bpca), 1L)
    expect_identical(getImputeMargin(impute_fun), 1L)
    expect_identical(getImputeMargin(impute_knn), 1L)
    expect_identical(getImputeMargin(impute_matrix), NA)
    expect_identical(getImputeMargin(impute_min), NA)
    expect_identical(getImputeMargin(impute_MinDet), 2L)
    expect_identical(getImputeMargin(impute_MinProb), 2L)
    expect_identical(getImputeMargin(impute_mixed), 1L)
    expect_identical(getImputeMargin(impute_mle), 2L)
    expect_identical(getImputeMargin(impute_neighbour_average), 1L)
    expect_identical(getImputeMargin(impute_QRILC), 2L)
    expect_identical(getImputeMargin(impute_RF), 2L)
    expect_identical(getImputeMargin(impute_with), NA)
    expect_identical(getImputeMargin(impute_zero), NA)
})

test_that("impute_MinDet(margin) works", {
    m <- matrix(1:9, ncol = 3)
    m[2, 2] <- NA
    m_imp <- impute_MinDet(m, margin = 1L, q = 0)
    expect_identical(m_imp[2, 2], 2)
    m_imp <- impute_MinDet(m, margin = 2L, q = 0)
    expect_identical(m_imp[2, 2], 4)
})

test_that("impute_neighbour_average(margin) works", {
    m <- matrix(1:9, ncol = 3)
    m[2, 2] <- NA
    m[1, 2] <- 10.0
    m_imp <- impute_neighbour_average(m, margin = 1L)
    expect_identical(m_imp[2, 2], 5)
    m_imp <- impute_neighbour_average(m, margin = 2L)
    expect_identical(m_imp[2, 2], 8)
})

test_that("impute_knn(margin) works", {
    m <- matrix(1:9, ncol = 3)
    m[1, ] <- m[2, ] + 0.5
    m[3, 2] <- 9
    m[2, 2] <- NA
    m_imp <- impute_knn(m, margin = 1L, k = 1)
    expect_identical(m_imp[2, 2], 5.5)
    m_imp <- impute_knn(m, margin = 2L, k = 1)
    expect_identical(m_imp[2, 2], 8)
})


test_that("impute_fun(margin) works", {
    m <- matrix(1:9, ncol = 3)
    m[2, 2] <- NA
    imp1 <- function(x) { x[is.na(x)] <- 1; x}
    imp2 <- function(x) { x[is.na(x)] <- 2; x}
    m_imp <- impute_fun(m, margin = 1L, FUN = imp1)
    expect_identical(m_imp[2, 2], 1)
    m_imp <- impute_fun(m, margin = 2L, FUN = imp2)
    expect_identical(m_imp[2, 2], 2)
})


test_that("impute_mixed(margin) works", {
    m <- matrix(1:12, ncol = 4)
    m[2:3, 2] <- m[3, 3] <- NA
    rand1 <- c(TRUE, TRUE, FALSE)
    rand2 <- c(FALSE, FALSE, TRUE, TRUE)
    m_imp <- impute_mixed(m, randna = rand1,
                          mar = "MinDet",
                          mnar = "zero",
                          margin = 1L)
    expect_true(m_imp[2, 2] > 0)
    expect_identical(m_imp[3, 2:3], c(0, 0))
    m_imp <- impute_mixed(m, randna = rand2,
                          mar = "MinDet",
                          mnar = "zero",
                          margin = 2L)
    expect_identical(m_imp[2:3, 2], c(0, 0))
    expect_true(m_imp[3, 3] > 0)
})


test_that("impute_bpca(margin) works", {
    m <- matrix(1:16, ncol = 4)
    m[2, 2] <- NA
    m_imp1 <- impute_bpca(m, margin = 1L)
    m_imp2 <- impute_bpca(m, margin = 2L)
    expect_identical(m_imp1[-2, -2], m_imp2[-2, -2])
    expect_true(m_imp1[2, 2] != m_imp2[2, 2])
})

test_that("impute_QRILC(margin) works", {
    m <- matrix(1:16, ncol = 4)
    m[2, 2] <- NA
    m_imp1 <- impute_QRILC(m, margin = 1L)
    m_imp2 <- impute_QRILC(m, margin = 2L)
    expect_identical(m_imp1[-2, -2], m_imp2[-2, -2])
    expect_true(m_imp1[2, 2] != m_imp2[2, 2])
})

test_that("impute_MinProb(margin) works", {
    m <- matrix(1:16, ncol = 4)
    m[2, 2] <- NA
    m_imp1 <- impute_MinProb(m, margin = 1L)
    m_imp2 <- impute_MinProb(m, margin = 2L)
    expect_identical(m_imp1[-2, -2], m_imp2[-2, -2])
    expect_true(m_imp1[2, 2] != m_imp2[2, 2])
})
