#' @title Create Edge List Matrix
#'
#' @description
#' This function creates a two-column matrix edge list of an undirected graph.
#'
#' @param x `numeric`, values to be matched, e.g. m/z from spectrum 1.
#' @param y `numeric`, values to be matched, e.g. m/z from spectrum 2.
#' @param tolerance `numeric`, accepted tolerance. Could be of length one or
#' the same length as `table`.
#' @param ppm `numeric(1)` representing a relative, value-specific
#' parts-per-million (PPM) tolerance that is added to `tolerance`.
#' @param na.rm `logical(1)` should rows with NA removed from the results?
#' (necessary for [`igraph::graph_from_edge_list()`].
#'
#' @return A two-column `matrix` with the undirected edge positions, for
#' [igraph::graph_from_edge_list()`] the indices in the `y` column have to be
#' increased by `length(x)` and the `NA` values (no match) have to be removed
#' manually.
#' @examples
#' x <- c(100.1, 100.2, 300, 500)
#' y <- c(100, 200, 299.9, 300.1, 505)
#' .edgeList(x, y, tolerance = 0.2)
.edgeList <- function(x, y, tolerance = 0, ppm = 0, na.rm = FALSE) {
    xy <- closest(x, y, tolerance = tolerance, ppm = ppm, duplicates = "keep")
    yx <- closest(y, x, tolerance = tolerance, ppm = ppm, duplicates = "keep")

    # switching the direction of the second match (yx) to allow using duplicated
    # to remove multiple edges (we use undirected graphs anyway)
    e <- cbind(x = c(seq_along(x), yx), y = c(xy, seq_along(y)))
    e[!duplicated(e), ]
}

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
