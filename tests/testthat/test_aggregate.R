
test_that("aggregation: medianPolish", {
    ## numerical example taken from ?stats::medpolish
    x <- rbind(c(14,15,14),
               c( 7, 4, 7),
               c( 8, 2,10),
               c(15, 9,10),
               c( 0, 2, 0))
    colnames(x) <- LETTERS[1:3]
    rownames(x) <- paste0("pep", 1:5)
    x2 <- medianPolish(x)
    expect_is(x2, "numeric")
    expect_identical(length(x2), ncol(x))
    mp <- stats::medpolish(x, trace.iter = FALSE)
    ## Check decomposition
    expect_identical(x, mp$overall + outer(mp$row, mp$col, "+") + mp$residuals)
    expect_identical(names(x2), colnames(x))
})


test_that("aggregation: robustSummary", {
    ## numeric example taken from `MSnbase::combineFeatures` on
    ## `log(filterNA(msnset), 2)`
    x <- structure(c(10.3961935744407, 10.6379251053134,
                     7.52885076885599, 11.1339832690524,
                     11.5154097311056, 7.69906817878979,
                     11.9394664781386, 12.2958526883634,
                     9.00858488668671, 12.9033445520186,
                     13.3390344671153, 9.75719265786117),
                   .Dim = 3:4,
                   .Dimnames = list(c("X1", "X52", "X53"),
                                    c("iTRAQ4.114", "iTRAQ4.115",
                                      "iTRAQ4.116", "iTRAQ4.117")))
    x2_expected <- c(iTRAQ4.114 = 9.52098981620336, iTRAQ4.115 = 10.1620299826269,
                     iTRAQ4.116 = 11.0813013510629, iTRAQ4.117 = 11.999857225665)
    x2 <- robustSummary(x)
    expect_equal(x2, x2_expected)
})

test_that("aggregation: robustSummary", {
    x <- structure(c(10.3961935744407, 10.6379251053134,
                     7.52885076885599, 11.1339832690524,
                     11.5154097311056, 7.69906817878979,
                     11.9394664781386, 12.2958526883634,
                     9.00858488668671, 12.9033445520186,
                     13.3390344671153, 9.75719265786117),
                   .Dim = 3:4)
    expect_error(robustSummary(x), "colnames must not be empty")
})

test_that("aggregation: aggregate_by_vector", {
    ## Numeric example taken from `MSnbase::combineFeatures` on
    ## `log(filterNA(msnset), 2)`
    x <- structure(c(3.37798349666944, 4.10151322208566, 3.81790550688852,
                     3.68373564094019, 3.41114487991947, 2.91242966348861, 1.97017675008189,
                     3.47689791527908, 4.04720220698125, 3.82566260214081, 3.57800373068237,
                     3.525493839628, 2.94468384643243, 1.98947385475541, 3.57766646528063,
                     3.94290011925067, 3.83197127627519, 3.48680275001825, 3.6200998807866,
                     3.17130049812522, 1.93779683012642, 3.68967315613391, 3.84557183862662,
                     3.82393074128306, 3.33967874492785, 3.73758233712817, 3.28646611488044,
                     1.92996581207855),
                   .Dim = c(7L, 4L),
                   .Dimnames = list(c("X1", "X27", "X41", "X47", "X52",
                                      "X53", "X55"),
                                    c("iTRAQ4.114", "iTRAQ4.115",
                                      "iTRAQ4.116", "iTRAQ4.117")))
    ## Different ways to provide INDEX
    k_char <- c("B", "E", "X", "E", "B", "B", "E")
    k_fact <- factor(k_char)
    k_fact2 <- factor(k_char, levels = c("X", "E", "B"))
    ## Harmonise row names for comparison - these can change based on
    ## the different levels.
    same_row_names <- c("B", "E", "X")

    ## aggregate: robustSummary
    x2_robust_expected <-
        structure(c(3.23385268002584, 3.27016773304649,
                    3.81790550688852, 3.33557123434545,
                    3.20489326413968, 3.82566260214081,
                    3.45635561473081, 3.12249989979845,
                    3.83197127627519, 3.57124053604751,
                    3.01422698197221, 3.82393074128306),
                  .Dim = 3:4,
                  .Dimnames = list(c("B", "E", "X"),
                                   c("iTRAQ4.114", "iTRAQ4.115",
                                     "iTRAQ4.116", "iTRAQ4.117")))
    ## Test for different INDEX types and order
    expect_equal(x2_robust_expected[same_row_names, ],
                 aggregate_by_vector(x, k_char, robustSummary)[same_row_names, ])
    expect_equal(x2_robust_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact, robustSummary)[same_row_names, ])
    expect_equal(x2_robust_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact2, robustSummary)[same_row_names, ])
    ## aggregate: medianPolish
    x2_medpolish_expected <-
        structure(c(3.36717083720277, 3.63886529932001,
                    3.81790550688852, 3.47689791527908,
                    3.57800373068237, 3.82566260214081,
                    3.57766646528063, 3.48680275001825,
                    3.83197127627519, 3.69360829441147,
                    3.38292391586096, 3.82393074128306),
                  .Dim = 3:4,
                  .Dimnames = list(c("B", "E", "X"),
                                   c("iTRAQ4.114", "iTRAQ4.115",
                                     "iTRAQ4.116", "iTRAQ4.117")))
    ## Test for different INDEX types and order
    expect_equal(x2_medpolish_expected[same_row_names, ],
                 aggregate_by_vector(x, k_char, medianPolish)[same_row_names, ])
    expect_equal(x2_medpolish_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact, medianPolish)[same_row_names, ])
    expect_equal(x2_medpolish_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact2, medianPolish)[same_row_names, ])
    ## aggregate: sum
    x2_sum_expected <-
        structure(c(9.70155804007753, 9.75542561310774,
                    3.81790550688852, 9.94707560133951,
                    9.61467979241903, 3.82566260214081,
                    10.3690668441924, 9.36749969939534,
                    3.83197127627519, 10.7137216081425,
                    9.11521639563303, 3.82393074128306),
                  .Dim = 3:4,
                  .Dimnames = list(c("B", "E", "X"),
                                   c("iTRAQ4.114", "iTRAQ4.115",
                                     "iTRAQ4.116", "iTRAQ4.117")))
    ## Test for different INDEX types and order
    expect_equal(x2_sum_expected[same_row_names, ],
                 aggregate_by_vector(x, k_char, colSums)[same_row_names, ])
    expect_equal(x2_sum_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact, colSums)[same_row_names, ])
    expect_equal(x2_sum_expected[same_row_names, ],
                 aggregate_by_vector(x, k_fact2, colSums)[same_row_names, ])
    
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/xhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        ## robustSummary
        hdf5out <- aggregate_by_vector(xhdf5, k_char, robustSummary)
        expect_true(inherits(hdf5out, "HDF5Matrix"))
        expect_equal(x2_robust_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact, robustSummary)
        expect_equal(x2_robust_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact2, robustSummary)
        expect_equal(x2_robust_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        ## medianPolish
        hdf5out <- aggregate_by_vector(xhdf5, k_char, medianPolish)
        expect_true(inherits(hdf5out, "HDF5Matrix"))
        expect_equal(x2_medpolish_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact, medianPolish)
        expect_equal(x2_medpolish_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact2, medianPolish)
        expect_equal(x2_medpolish_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        ## Matrix::colSums
        hdf5out <- aggregate_by_vector(xhdf5, k_char, Matrix::colSums)
        expect_true(inherits(hdf5out, "HDF5Matrix"))
        expect_equal(x2_sum_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact, Matrix::colSums)
        expect_equal(x2_sum_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        hdf5out <- aggregate_by_vector(xhdf5, k_fact2, Matrix::colSums)
        expect_equal(x2_sum_expected[same_row_names, ],
                     as.matrix(hdf5out)[same_row_names, ])
        ## Remove file
        unlink(tmpf)
    }
})


test_that("aggregation: colCounts", {
    ## Simple case with NAs
    m <- matrix(c(1, NA, 2, 3, NA, NA, 4, 5, 6),
                nrow = 3)
    expect_identical(colCounts(m), c(2, 1, 3))
    ## No NAs
    m <- matrix(rnorm(30), nrow = 3)
    expect_identical(colCounts(m), rep(3, 10))
    ## NAs along diagonal
    m <- matrix(rnorm(25), nrow = 5)
    diag(m) <- NA
    expect_identical(colCounts(m), rep(4, 5))
    ## Only NAs
    m <- matrix(NA, ncol = 5, nrow = 3)
    expect_identical(colCounts(m), rep(0, 5))
    ## NaN instead of NA
    m <- matrix(rnorm(25), nrow = 5)
    m[1,1] <- NaN
    expect_identical(colCounts(m), c(4, rep(5, 4)))
    ## NA and Inf
    m[2,2] <- Inf
    expect_identical(colCounts(m), c(4, rep(5, 4)))
    
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Test HDF5Matrix compatibility
        tmpf <- paste0(tempdir(), "/mhdf5")
        m <- matrix(c(1, NA, 2, 3, NA, NA, 4, 5, 6),
                    nrow = 3)
        mhdf5 <- HDF5Array::writeHDF5Array(m, tmpf, with.dimnames = TRUE)
        expect_identical(colCounts(mhdf5), c(2, 1, 3))
        unlink(tmpf)
    }
})


test_that("aggregate_by_matix works", {
    ## numerical example taken from ?stats::medpolish
    x <- rbind(c(14,15,14),
               c( 7, 4, 7),
               c( 8, 2,10),
               c(15, 9,10),
               c( 0, 2, 0))
    colnames(x) <- paste0("S", 1:3)
    rownames(x) <- paste0("pep", 1:5)
    ## aggregation index
    k <- paste0("Prot", rep(1:2, c(2, 3)))
    ## adjacency matrix
    adj <- matrix(c(1, 1, 0, 0, 0,
                    0, 0, 1, 1, 1),
                  ncol = 2)
    rownames(adj) <- rownames(x)
    colnames(adj) <- unique(k)
    ## aggregation by sum
    av1 <- aggregate_by_vector(x, k, colSums)
    am1 <- aggregate_by_matrix(x, adj, colSumsMat)
    expect_identical(av1, am1)
    ## aggregation by mean
    av2 <- aggregate_by_vector(x, k, colMeans)
    am2 <- aggregate_by_matrix(x, adj, colMeansMat)
    expect_identical(av2, am2)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        ## colSumsMat
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colSumsMat)
        expect_true(inherits(amhdf5, "HDF5Matrix"))
        expect_identical(am1, as.matrix(amhdf5))
        ## colMeansMat
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colMeansMat)
        expect_true(inherits(amhdf5, "HDF5Matrix"))
        expect_identical(am2, as.matrix(amhdf5))
        ## Remove file
        unlink(tmpf)
    }
})


test_that("aggregate_by_matix works with NAs", {
    ## numerical example taken from ?stats::medpolish
    x <- rbind(c(14,15,14),
               c( 7, 4, 7),
               c( 8, 2,10),
               c(15, 9,10),
               c( 0, 2, 0))
    colnames(x) <- paste0("S", 1:3)
    rownames(x) <- paste0("pep", 1:5)
    x[1, 1] <- x[4, 2] <- NA
    ## aggregation index
    k <- paste0("Prot", rep(1:2, c(2, 3)))
    ## adjacency matrix
    adj <- matrix(c(1, 1, 0, 0, 0,
                    0, 0, 1, 1, 1),
                  ncol = 2)
    rownames(adj) <- rownames(x)
    colnames(adj) <- unique(k)
    ## aggregation by sum, na.rm = FALSE
    av1 <- aggregate_by_vector(x, k, colSums)
    am1 <- aggregate_by_matrix(x, adj, colSumsMat)
    expect_identical(av1, am1)
    ## aggregation by mean, na.rm = FALSE
    av2 <- aggregate_by_vector(x, k, colMeans)
    am2 <- aggregate_by_matrix(x, adj, colMeansMat)
    expect_identical(av2, am2)
    ## aggregation by sum, na.rm = TRUE
    av3 <- aggregate_by_vector(x, k, colSums, na.rm = TRUE)
    am3 <- aggregate_by_matrix(x, adj, colSumsMat, na.rm = TRUE)
    expect_identical(av3, am3)
    ## aggregation by mean, na.rm = TRUE
    av4 <- aggregate_by_vector(x, k, colMeans, na.rm = TRUE)
    am4 <- aggregate_by_matrix(x, adj, colMeansMat, na.rm = TRUE)
    expect_identical(av4, am4)
    ## Test HDF5Matrix compatibility
    if (requireNamespace("HDF5Array", quietly = TRUE)) {
        ## Data in HDF5
        tmpf <- paste0(tempdir(), "/mhdf5")
        xhdf5 <- HDF5Array::writeHDF5Array(x, tmpf, with.dimnames = TRUE)
        ## aggregation by sum, na.rm = FALSE
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colSumsMat)
        expect_identical(am1, as.matrix(amhdf5))
        ## aggregation by mean, na.rm = FALSE
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colMeansMat)
        expect_identical(am2, as.matrix(amhdf5))
        ## aggregation by sum, na.rm = TRUE
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colSumsMat, na.rm = TRUE)
        expect_identical(am3, as.matrix(amhdf5))
        ## aggregation by mean, na.rm = TRUE
        amhdf5 <- aggregate_by_matrix(xhdf5, adj, colMeansMat, na.rm = TRUE)
        expect_identical(am4, as.matrix(amhdf5))
        ## Remove file
        unlink(amhdf5)
    }
})

