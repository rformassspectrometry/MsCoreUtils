#' @rdname aggregate
#'
#' @export
colMeansMat <- function(x, MAT)
  colSumsMat(x, MAT)/colSums(MAT)

#' @rdname aggregate
#'
#' @export
colSumsMat <- function(x, MAT)
  t(MAT) %*% x


##' @param MAT An adjacency matrix that defines what features with
##'     `nrow(MAT) == nrow(x)`
##'
##' @return [aggregate_by_matrix()] returns a `matrix` (or `Matrix`)
##'     of dimensions `ncol(MAT)` and `ncol(x), with `dimnames` equal
##'     to `colnames(x)` and `rownames(MAT)`.
##'
##' @rdname aggregate
##'
##' @export
aggregate_by_matrix <- function(x, MAT, FUN, ...) {
    if (!inherits(x, "matrix"))
        stop("'x' must be a matrix.")
    if (!identical(nrow(MAT), nrow(x)))
        stop("nrow(MAT) must be identical to 'nrow(x).")
    res <- do.call(FUN, list(x, MAT))
    if (!is.null(colnames(MAT)))
        stopifnot(identical(colnames(MAT), rownames(res)))
    if (!is.null(colnames(x)))
        stopifnot(identical(colnames(x), colnames(res)))
    res
}
