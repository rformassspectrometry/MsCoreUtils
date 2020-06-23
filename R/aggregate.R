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


##' @title Aggreagate quantitative features.
##' 
##' @description
##' This function takes a matrix of quantitative features `x` and a
##' factor (of length equal to `nrow(x)`) defining subsets, and
##' applies a user-defined function to aggregate each subset into a
##' vector of quantitative values.
##'
##' User-defined functions must thus return a vector of length equal
##' to `ncol(x)`. Examples thereof are
##'
##' - [medianPolish()] to fits an additive model (two way decomposition)
##'   using Tukey's median polish_ procedure using
##'   [stats::medpolish()];
##'
##' - [robustSummary()] to calculate a robust aggregation using
##'   [MASS::rlm()];
##'
##' - [base::colMeans()][base::colSums()] to use the mean of each column;
##'
##' - [base::colSums()] to use the sum of each column;
##'
##' - [matrixStats::colMedians()] to use the median of each column.
##'
##' @param x A `matrix` of mode `numeric`. 
##' @param INDEX A `factor` of length `nrow(x)`.
##' @param FUN A `function` to be applied to the subsets of `x`.
##' @param ... Additional arguments passed to `FUN`.
##' @return A new `matrix` of dimensions `ncol(x)` and `length(INDEX)`
##'     with `dimnames` equal to `colnames(x)` and `INDEX`.
##' 
##' @author Laurent Gatto
##'
##' @family Quantitative feature aggregation
##' 
##' @export
##' 
##' @examples
##'
##' x <- structure(c(10.3961935744407, 17.1663715212693, 14.1027587989326,
##'                 12.850349037785, 10.6379251053134, 7.52885076885599,
##'                 3.91816118984218, 11.1339832690524, 16.5321471730746,
##'                 14.1787908569268, 11.9422579479634, 11.5154097311056,
##'                 7.69906817878979, 3.97092153807337, 11.9394664781386,
##'                 15.3791100898935, 14.2409281956285, 11.2106867261254,
##'                 12.2958526883634, 9.00858488668671, 3.83120129974963,
##'                 12.9033445520186, 14.375814954807, 14.1617803596661,
##'                 10.1237981632645, 13.3390344671153, 9.75719265786117,
##'                 3.81046169359919),
##'               .Dim = c(7L, 4L),
##'               .Dimnames = list(c("X1", "X27", "X41", "X47", "X52",
##'                                  "X53", "X55"),
##'                                c("iTRAQ4.114", "iTRAQ4.115",
##'                                  "iTRAQ4.116", "iTRAQ4.117")))
##' x
##' 
##' k <- factor(c("B", "E", "X", "E", "B", "B", "E"))
##'
##' aggregate_by_vector(x, k, colMeans)
##' aggregate_by_vector(x, k, robustSummary)
##' aggregate_by_vector(x, k, medianPolish)
aggregate_by_vector <- function(x, INDEX, FUN, ...) {
    if (!inherits(x, "matrix"))
        stop("'x' must be a matrix.")
    if (!identical(length(INDEX), nrow(x)))
        stop("The length of 'INDEX' has to be identical to 'nrow(x).")
    INDEX <- factor(INDEX)
    res <- tapply(
        seq_len(nrow(x)),
        INDEX,
        FUN = function(i) FUN(x[i, , drop = FALSE], ...),
        simplify = FALSE
    )
    res <- do.call(rbind, res)
    rownames(res) <- levels(INDEX)
    colnames(res) <- colnames(x)
    res
}

## aggregate_by_list <- function(x, INDEX, FUN, ...) {    
## }
