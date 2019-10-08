#' @title (Re)order edges
#'
#' @description
#' Ensures matrix/list with edges is ordered increasingly and gaps are filled
#' with `NA`
#'
#' @param x `numeric`, values to be matched, e.g. m/z from spectrum 1.
#' @param y `numeric`, values to be matched, e.g. m/z from spectrum 2.
#' @param e `list`, of length two (`x`, `y`) with edges
#'
#' @return A `list` with two columns, namely `x` and `y`,
#' representing the index of the values in `x` matching the corresponding value
#' in `y` (or `NA` if the value do not match).
#'
#' @examples
#' x <- c(100.1, 100.2, 300, 500)
#' y <- c(100, 200, 299.9, 300.1, 505)
#' e <- .edgeList(x, y, tolerance = 0.2)
#' .orderEdges(x, y, e)
.orderEdges <- function(x, y, e) {
    na <- is.na(e[[1L]])
    xe <- x[e[[1L]]]
    xe[na] <- y[e[[2L]][na]]
    o <- order(xe, method = "radix")
    list(x = e[[1L]][o], y = e[[2L]][o])
}
