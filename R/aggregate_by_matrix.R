#' @rdname aggregate
#' 
#' @param na.rm A `logical(1)` indicating whether the missing values 
#'     (including NaN) should be omitted from the calculations or not.
#'     Defaults to `FALSE`.
#'
#' @export
colMeansMat <- function(x, MAT, na.rm = FALSE) {
    if (na.rm) {
        n <- Matrix::crossprod(MAT, !is.na(x))
    } else {
        n <- Matrix::colSums(MAT)
    }
    colSumsMat(x, MAT, na.rm = na.rm) / n
}
  

#' @rdname aggregate
#'
#' @export
colSumsMat <- function(x, MAT, na.rm = FALSE) {
    if (na.rm) {
        x[is.na(x)] <- 0
        res <- Matrix::crossprod(MAT, x)
    } else {
        ## Locate the missing data post-aggregation
        xna <- Matrix::crossprod(MAT, is.na(x))
        ## Avoid NAs to propagate to the whole column
        x[is.na(x)] <- 0
        res <- Matrix::crossprod(MAT, x)
        ## Replace aggregated values that should be missing
        res[xna == 1] <- NA
    }
    res
}
  


##' @param MAT An adjacency matrix that defines peptide-protein
##'     relations with `nrow(MAT) == nrow(x)`: a non-missing/non-null
##'     value at position (i,j) indicates that peptide i belong to
##'     protein j. This matrix is tyically binary but can also contain
##'     weighted relations.
##'
##' @return [aggregate_by_matrix()] returns a `matrix` (or `Matrix`)
##'     of dimensions `ncol(MAT)` and `ncol(x), with `dimnames` equal
##'     to `colnames(x)` and `rownames(MAT)`.
##'
##' @rdname aggregate
##'
##' @importFrom methods is
##'
##' @export
aggregate_by_matrix <- function(x, MAT, FUN, ...) {
    if (!(is.matrix(x) | inherits(x, "HDF5Matrix")))
        stop("'x' must be a matrix or an object that inherits from ",
             "'HDF5Matrix'.")
    if (!is(MAT, "Matrix") && !is(MAT, "matrix"))
        stop("'MAT' must be a matrix.")
    if (!identical(nrow(MAT), nrow(x)))
        stop("nrow(MAT) must be identical to 'nrow(x).")
    res <- do.call(FUN, list(x, MAT, ...))
    if (!is.null(colnames(MAT)) && !identical(colnames(MAT), rownames(res)))
        stop("The colum names of 'MAT' have to be identical to the row names of 'res'!")
    if (!is.null(colnames(x)) && !identical(colnames(x), colnames(res)))
        stop("The column names of 'x' have to be identical to the column names of 'res'!")
    colnames(res) <- colnames(x)
    if (inherits(x, "HDF5Matrix"))
        res <- HDF5Array::writeHDF5Array(res, 
                                         filepath = HDF5Array::path(x),
                                         with.dimnames = TRUE)
    res
}
