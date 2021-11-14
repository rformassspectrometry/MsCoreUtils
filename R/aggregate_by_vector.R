##' @title Return the Robust Expression Summary of a matrix
##'
##' @description
##' This function calculates the robust summarisation for each feature
##' (protein). Note that the function assumes that the intensities in
##' input `e` are already log-transformed.
##'
##' @param x A feature by sample `matrix` containing quantitative
##'     data with mandatory `colnames` and `rownames`.
##' @param ... Additional arguments passed to [MASS::rlm()].
##' @return `numeric()` vector of length `ncol(x)` with robust
##'     summarised values.
##'
##' @author Adriaan Sticker, Sebastian Gibb and Laurent Gatto
##'
##' @family Quantitative feature aggregation
##'
##' @export
##'
##' @importFrom MASS rlm
##' @importFrom stats model.matrix .lm.fit
##'
##' @examples
##' x <- matrix(rnorm(30), nrow = 3)
##' colnames(x) <- letters[1:10]
##' rownames(x) <- LETTERS[1:3]
##' robustSummary(x)
robustSummary <- function(x, ...) {
    if (is.null(colnames(x)))
        stop("colnames must not be empty.")
    if (is.null(rownames(x)))
        stop("rownames must not be empty.")

    ## If there is only one 1 peptide for all samples return
    ## expression of that peptide
    if (nrow(x) == 1L) return(x)

    ## remove missing values
    p <- !is.na(x)
    expression <- x[p] ## expression becomes a vector
    sample <- rep(colnames(x), each = nrow(x))[p]
    feature <- rep(rownames(x), times = ncol(x))[p]

    ## model.matrix breaks on factors with 1 level so make vector of
    ## ones (intercept).
    if (length(unique(sample)) == 1L) sample <- rep(1, length(sample))

    ## Sum contrast on peptide level so sample effect will be mean
    ## over all peptides instead of reference level.
    X <- model.matrix(~ -1 + sample + feature,
                      contrasts.arg = list(feature = 'contr.sum'))
    ## MASS::rlm breaks on singulare values.
    ## - Check with base lm if singular values are present.
    ## - If so, these coefficients will be zero, remove this collumn
    ##   from model matrix
    ## - Rinse and repeat on reduced modelmatrix till no singular
    ##   values are present
    repeat {
        fit <- .lm.fit(X, expression)
        id <- fit$coefficients != 0
        X <- X[ , id, drop = FALSE]
        if (all(id)) break
    }
    ## Last step is always rlm: calculate estimated effects as
    ## summarised values
    fit <- rlm(X, expression, ...)

    sampleid <- seq_along(unique(sample))
    ## This will be needed for NUSE-type of quality control, but will
    ## need to check for missing data as below.
    ## se <- unique(summary(fit)$coefficients[sampleid, 'Std. Error'])

    ## Take the sample coefficients ( = summarised expression values)
    coef  <-  fit$coefficients[sampleid]
    ## Sort the sample coefficients in the same way as the samplenames
    ## of expression matrix. Puts NA for the samples without any
    ## expression value
    res <- coef[paste0('sample', colnames(x))]
    names(res) <- colnames(x)
    res
}

##' @title Return the Median Polish (Robust Twoway Decomposition) of a matrix
##'
##' @description
##' Fits an additive model (two way decomposition) using Tukey's median
##' polish procedure using [stats::medpolish()].
##'
##' @param x A `matrix` of mode `numeric`.
##'
##' @param verbose Default is `FALSE`.
##'
##' @param ... Additional arguments passed to [stats::medpolish()].
##'
##' @return A `numeric` vector of length identical to `ncol(x)`.
##'
##' @author Laurent Gatto
##'
##' @family Quantitative feature aggregation
##'
##' @export
##'
##' @importFrom stats medpolish
##'
##' @examples
##' x <- matrix(rnorm(30), nrow = 3)
##' medianPolish(x)
medianPolish <- function(x, verbose = FALSE, ...) {
    medpol <- stats::medpolish(x, trace.iter = verbose, ...)
    medpol$overall + medpol$col
}

##' @title Counts the number of features
##'
##' @description
##' Returns the number of non-NA features in a features by sample
##' matrix.
##'
##' @param x A `matrix` of mode `numeric`.
##'
##' @param ... Currently ignored.
##'
##' @return A `numeric` vector of length identical to `ncol(x)`.
##'
##' @family Quantitative feature aggregation
##'
##' @export
##'
##' @author Laurent Gatto
##'
##' @examples
##' m <- matrix(c(1, NA, 2, 3, NA, NA, 4, 5, 6),
##'             nrow = 3)
##' colCounts(m)
##' m <- matrix(rnorm(30), nrow = 3)
##' colCounts(m)
colCounts <- function(x, ...)
    colSums(!is.na(x))



##' @param x A `matrix` of mode `numeric`.
##'
##' @param INDEX A `vector` or `factor` of length `nrow(x)`.
##'
##' @param FUN A `function` to be applied to the subsets of `x`.
##'
##' @param ... Additional arguments passed to `FUN`.
##'
##' @return [aggregate_by_vector()] returns a new `matrix` of
##'     dimensions `length(INDEX)` and `ncol(x), with `dimnames` equal
##'     to `colnames(x)` and `INDEX`.
##'
##' @rdname aggregate
##'
##' @export
aggregate_by_vector <- function(x, INDEX, FUN, ...) {
    if (!inherits(x, "matrix"))
        stop("'x' must be a matrix.")
    if (!identical(length(INDEX), nrow(x)))
        stop("The length of 'INDEX' has to be identical to 'nrow(x).")
    INDEX <- factor(INDEX)
    FUN <- match.fun(FUN)
    res <- lapply(
        split(seq_len(nrow(x)), INDEX),
        FUN = function(i) FUN(x[i, , drop = FALSE], ...)
    )
    res <- do.call(rbind, res)
    rownames(res) <- levels(INDEX)
    colnames(res) <- colnames(x)
    res
}
