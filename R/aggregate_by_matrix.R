#' @rdname aggregate
#'
#' @export
colMeansMat <- function(x, MAT)
  colSumsMat(x, MAT) / colSums(MAT)

#' @rdname aggregate
#'
#' @export
colSumsMat <- function(x, MAT)
  Matrix::crossprod(MAT, x)


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
    if (!is.matrix(x))
        stop("'x' must be a matrix.")
    if (!is(MAT, "Matrix") && !is(MAT, "matrix"))
        stop("'MAT' must be a matrix.")
    if (!identical(nrow(MAT), nrow(x)))
        stop("nrow(MAT) must be identical to 'nrow(x).")
    res <- do.call(FUN, list(x, MAT, ...))
    if (!is.null(colnames(MAT)) && !identical(colnames(MAT), rownames(res)))
        stop("The colum names of 'MAT' have to be identical to the row names of 'res'!")
    if (!is.null(colnames(x)) && !identical(colnames(x), colnames(res)))
        stop("The column names of 'x' have to be identical to the column names of 'res'!")
    res
}
