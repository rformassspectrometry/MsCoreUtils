#' @rdname matching
#'
#' @param FUN `function`, similarity function that should be maximized.
#' @param \dots further arguments passed to `FUN`.
#'
#' @details
#' `joinGraph`: joins two `matrix` by mapping values in `x` with
#' values in `y` and *vice versa* if they are similar enough (provided the
#' `tolerance` and `ppm` specified). For multiple matches in `x` or `y` all
#' possible combinations are evaluated using the similarity function `FUN`. The
#' combination that yield the highest return value of `FUN` is used for the final
#' match.
#'
#' @return `joinGraph` returns a `list` with two columns, namely `x` and `y`,
#' representing the index of the values in `x` matching the corresponding value
#' in `y` (or `NA` if the value does not match).
#'
#' @author `joinGraph`: Sebastian Gibb, Thomas Naake
#' @export
#' @examples
#'
#' x <- matrix(
#'      c(100.001, 100.002, 300.01, 300.02, 1, 9, 1, 9),
#'        ncol = 2L, dimnames = list(c(), c("mz", "intensity"))
#' )
#' y <- matrix(
#'     c(100.0, 200.0, 300.002, 300.025, 300.0255, 9, 1, 1, 9, 1),
#'     ncol = 2L, dimnames = list(c(), c("mz", "intensity"))
#' )
#' joinGraph(x, y, ppm = 20)
joinGraph <- function(x, y, tolerance = 0, ppm = 0, FUN = dotproduct, ...) {
    validPeaksMatrix(x)
    validPeaksMatrix(y)
    FUN <- match.fun(FUN)

    e <- .edgeList(x[, 1L], y[, 1L], tolerance = tolerance, ppm = ppm)
    e <- .orderEdges(x[, 1L], y[, 1L], e)
    g <- .edgeGroups(e)
    ei <- .edgeGroupFrom(e, g)
    gi <- which(.isPrecursorIdentical(g) | .isFollowerIdentical(g))

    if (!length(gi))
        return(e)

    cmb <- .combinations(g[gi])

    namask <- edges <- cbind(e[[1L]], e[[2L]])
    namask[cbind(gi, ei[gi])] <- NA_real_

    score <- vapply1d(cmb, function(i) {
        ii <- namask
        ii[gi[i],] <- edges[gi[i],]
        xx <- x[ii[, 1L],]
        yy <- y[ii[, 2L],]
        if (.anyCrossing(xx[, 1L], yy[, 1L]))
            0
        else
            FUN(xx, yy, ...)
    })
    cmb <- cmb[[which.max(score)]]

    namask[gi[cmb],] <- edges[gi[cmb],]
    list(x = namask[, 1L], y = namask[, 2L])
}

#' @title Crossing Edges
#'
#' @description
#' This function tests for crossing edges.
#'
#' @param x `numeric`
#' @param y `numeric`
#'
#' @return `logical`, `TRUE` if at least one crossing edge was found, otherwise
#' `FALSE`.
#' @author Sebastian Gibb
#' @noRd
#' @examples
#' .anyCrossing(x = 1:3, y = c(NA, 1:2))
#' .anyCrossing(x = 1:3, y = c(2, 1, NA))
.anyCrossing <- function(x, y) {
    is.unsorted(x, na.rm = TRUE, strictly = TRUE) ||
        is.unsorted(y, na.rm = TRUE, strictly = TRUE)
}

#' @title Find Edge Groups
#'
#' @description
#' This function finds edges that belong to the same group. A group is definied
#' by at least one identical point for following edges. It assumes that the
#' edge list is ordered.
#'
#' @param e `list` with edges
#' @return `integer` group values
#' @author Sebastian Gibb
#' @noRd
#' @examples
#' .edgeGroups(list(x = c(1, 2, NA, 3, 4, 4, 5), y = c(1, 1, 2, 3, 3, 4, 4)))
.edgeGroups <- function(e) {
    n <- lengths(e)
    if (!is.list(e) || n[1L] != n[2L])
        stop("'e' has to be a list with two elements of equal length.")

    px <- .isPrecursorIdentical(e[[1L]])
    py <- .isPrecursorIdentical(e[[2L]])

    for (i in seq_along(px)) {
        if (px[i] && py[i - 1L])
            px[i] <- FALSE
        if (py[i] && px[i - 1L])
            py[i] <- FALSE
    }
    cumsum(!(px | py))
}

#' @title Find Origin of Edge Group
#'
#' @description
#' Finds the index of the list (x or y) to which the group belongs to.
#'
#' @param e `list`, edge list
#' @param g `numeric` group vector
#'
#' @return `numeric`, `1` if `x` was lower than `y`, otherwise `2`. If `x` ==
#' `y` the decision of the previous/next element is returned
#'
#' @author Sebastian Gibb
#' @seealso .edgeGroups
#'
#' @noRd
#' @examples
#' e <- list(x = c(1, 2, NA, 3, 4, 4, 5), y = c(1, 1, 2, 3, 3, 4, 4))
#' .edgeGroupFrom (e$x, e$y)
.edgeGroupFrom <- function(e, g = .edgeGroups(e)) {
    if (!is.list(e) || length(e[[1L]]) != length(e[[2L]]))
        stop("'e' has to be a list with two elements of equal length.")
    if (length(e[[1L]]) != length(g))
        stop("'g' has to be of the same length as the elements in 'e'.")

    2L - (
        (!is.na(e[[1L]]) & is.na(e[[2L]])) |
        (.isPrecursorIdentical(e[[1L]]) & .isPrecursorIdentical(g)) |
        (.isFollowerIdentical(e[[1L]]) & .isFollowerIdentical(g))
    )
}

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
#' @return A `list` with the undirected edge positions, for
#' [igraph::graph_from_edge_list()`] the `list` has to be `rbind`ed, the
#' indices in the `y` column have to be increased by `length(x)` and the `NA`
#' values (no match) have to be removed manually.
#'
#' @author Sebastian Gibb
#'
#' @importFrom stats setNames
#' @noRd
#' @examples
#' x <- c(100.1, 100.2, 300, 500)
#' y <- c(100, 200, 299.9, 300.1, 505)
#' .edgeList(x, y, tolerance = 0.2)
.edgeList <- function(x, y, tolerance = 0, ppm = 0) {
    xy <- closest(x, y, tolerance = tolerance, ppm = ppm, duplicates = "keep")
    yx <- closest(y, x, tolerance = tolerance, ppm = ppm, duplicates = "keep")

    # switching the direction of the second match (yx) to allow using duplicated
    # to remove multiple edges (we use undirected graphs anyway)
    e <- mapply(c, c(seq_along(x), yx),  c(xy, seq_along(y)), SIMPLIFY = FALSE)
    e <- e[!duplicated(e)]
    setNames(.transposeList(e), c("x", "y"))
}

#' @title (Re)order edges
#'
#' @description
#' Ensures list with edges is ordered increasingly and gaps are filled
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
#' @author Sebastian Gibb
#' @noRd
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
