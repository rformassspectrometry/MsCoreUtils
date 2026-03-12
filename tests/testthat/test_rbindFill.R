test_that("rbindFill works", {
    require("S4Vectors")
    expect_error(rbindFill(1:2, 1:4), "just works for")

    ## matrix
    a <- matrix(1:9, nrow = 3, ncol = 3)
    colnames(a) <- c("a", "b", "c")
    b <- matrix(1:12, nrow = 3, ncol = 4)
    colnames(b) <- c("b", "a", "d", "e")
    res <- rbindFill(a, b)
    expect_equal(colnames(res), c("a", "b", "c", "d", "e"))
    expect_equal(class(res), class(a))
    expect_equal(res[, "a"], c(a[, "a"], b[, "a"]))
    expect_equal(res[, "b"], c(a[, "b"], b[, "b"]))
    expect_equal(res[, "d"], c(NA, NA, NA, b[, "d"]))

    res2 <- rbindFill(list(a, b))
    expect_identical(res, res2)

    res <- rbindFill(a, b[, c("b", "a")])
    expect_equal(colnames(res), c("a", "b", "c"))
    expect_equal(res[, "a"], c(a[, "a"], b[, "a"]))

    ## DataFrame
    a <- DataFrame(a = 1:4, b = FALSE, c = letters[1:4])
    b <- DataFrame(d = 1:4, b = TRUE)
    res <- rbindFill(a, b)
    expect_equal(colnames(res), c("a", "b", "c", "d"))
    expect_equal(res$a, c(1:4, NA, NA, NA, NA))
    expect_equal(res$b, rep(c(FALSE, TRUE), each = 4))

    b$e <- Rle(1, 4)
    res <- rbindFill(a, b)
    expect_identical(res$e, Rle(c(NA_integer_, 1), c(4, 4)))

    a$e <- Rle(2, 4)
    res <- rbindFill(a, b)
    expect_identical(res$e, Rle(c(2, 1), c(4, 4)))

    res <- rbindFill(a, b, DataFrame(z = factor(1:5)))
    expect_identical(res$z, factor(c(rep(NA, nrow(a) + nrow(b)), 1:5)))
    expect_identical(res$a, c(1:4, rep(NA, nrow(b) + 5)))
    expect_identical(res$b, c(rep(FALSE, nrow(a)), rep(TRUE, nrow(b)), rep(NA, 5)))

    ## DataFrame containing SimpleList.
    a$mz <- list(1:3, 1:4, 1:2, 1:3)
    res <- rbindFill(a, b)
    expect_true(is(res$mz, "list"))
    expect_true(all(unlist(is.na(res$mz[5:8]))))
    expect_true(is.na(res$mz[[5]]))

    ## If the first DataFrame doesn't contain a SimpleList but the second one
    res <- DataFrame(d = c(1L:4L, rep(NA_real_, 4L)),
                     b = rep(c(TRUE, FALSE), each = 4L),
                     e = Rle(1:2, 4),
                     a = c(rep(NA_real_, 4L), 1L:4L),
                     c = c(rep(NA_character_, 4L), letters[1L:4L]))
    res$mz <- list(NA, NA, NA, NA,
                   1:3, 1:4, 1:2, 1:3)
    expect_equal(rbindFill(b, a), res)

    ## Ensure data types are correct after merging.
    a <- DataFrame(int = c(1L, 1L), int_rle = Rle(c(1L, 1L)),
                   log = c(TRUE, FALSE), log_rle = Rle(c(FALSE, FALSE)))
    b <- DataFrame(real = c(1.2, 1.5), real_rle = Rle(c(1.2, 1.5)))
    res <- rbindFill(a, b)
    expect_true(is.integer(res$int))
    expect_true(is.integer(res$int_rle@values))
    expect_true(is.logical(res$log))
    expect_true(is.logical(res$log_rle@values))
    expect_true(is.double(res$real))
    expect_true(is.double(res$real_rle@values))

    ## works with a single DataFrame or data.frame
    expect_equal(rbindFill(a), a)
    b <- data.frame(a = 1:3)
    expect_equal(rbindFill(b), b)

    ## data.frame
    a <- data.frame(char = c("a", "b", "c"), int = c(1L, 2L, 3L),
                    num = c(1.23, 1.45, 1.2))
    b <- data.frame(int = c(4L, 5L), char = c("d", "e"))
    res <- rbindFill(a, b)
    expect_equal(res$char, c("a", "b", "c", "d", "e"))
    expect_equal(res$int, c(1L, 2L, 3L, 4L, 5L))
    expect_equal(res$num, c(1.23, 1.45, 1.2, NA_real_, NA_real_))

    res <- rbindFill(b, a)
    expect_equal(res$char, c("d", "e", "a", "b", "c"))
    expect_equal(res$int, c(4L, 5L, 1L, 2L, 3L))
    expect_equal(res$num, c(NA_real_, NA_real_, 1.23, 1.45, 1.2))

    ## only matrices
    a <- matrix(1:10, ncol = 2, nrow = 5)
    colnames(a) <- c("A", "B")
    b <- matrix(1:10, ncol = 5, nrow = 2)
    colnames(b) <- c("D", "E", "B", "F", "A")
    d <- matrix(TRUE, ncol = 3, nrow = 2)
    colnames(d) <- c("B", "F", "E")
    res <- rbindFill(a, b, d)
    expect_true(is.matrix(res))
    expect_equal(colnames(res), c("A", "B", "D", "E", "F"))
    expect_true(is.integer(res[, 1L]))
})

test_that(".rbind_fill_matrix works", {
    a <- matrix(1:10, ncol = 2, nrow = 5)
    colnames(a) <- c("A", "B")
    b <- matrix(1:10, ncol = 5, nrow = 2)
    colnames(b) <- c("D", "E", "B", "F", "A")
    d <- matrix(TRUE, ncol = 3, nrow = 2)
    colnames(d) <- c("B", "F", "E")

    res <- .rbind_fill_matrix(list(a, b, d))
    expect_true(is.matrix(res))
    expect_true(is.integer(res[, 1L]))
    expect_equal(colnames(res), c("A", "B", "D", "E", "F"))
    expect_equal(res[1:nrow(a), c("A", "B")], a)
    expect_true(all(is.na(res[1:nrow(a), c("D", "E", "F")])))
    expect_equal(res[6:7, ], b[, c("A", "B", "D", "E", "F")])
    expect_true(all(res[8:9, c("B", "F", "E")] == 1L))

    ## empty matrix
    e <- matrix(ncol = 2, nrow = 0)
    expect_error(.rbind_fill_matrix(list(a, e)), "column names")
    colnames(e) <- c("Z", "Y")
    res <- .rbind_fill_matrix(list(a, e))
    expect_true(is.matrix(res))
    expect_equal(colnames(res), c("A", "B", "Z", "Y"))
    expect_equal(res[, c("A", "B")], a)

    ## no colnames
    z <- matrix(1:10, ncol = 2)
    expect_error(.rbind_fill_matrix(list(a, z)), "column names")
})
