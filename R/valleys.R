#' @title Find Peak Valleys
#'
#' @description
#' This function finds the valleys around peaks.
#'
#' @param x `numeric`, e.g. intensity values.
#'
#' @param p `integer`, indices of identified peaks/local maxima.
#'
#' @return A `matrix` with three columns representing the index of the left
#' valley, the peak centroid, and the right valley.
#'
#' @note
#' The detection of the valleys is based on [`localMaxima`]. It returns the
#' *first* occurence of a local maximum (in this specific case the minimum).
#' For plateaus, e.g. `c(0, 0, 0, 1:3, 2:1, 0)` this results in a wrongly
#' reported left valley index of `1` (instead of `3`, see the example section as
#' well). In real data this should not be a real problem.
#' `x[x == min(x)] <- Inf` could be used before running `valleys` to circumvent
#' this specific problem but it is not really tested and could cause different
#' problems.
#'
#' @author Sebastian Gibb
#' @export
#' @examples
#' ints <- c(5, 8, 12, 7, 4, 9, 15, 16, 11, 8, 3, 2, 3, 2, 9, 12, 14, 13, 8, 3)
#' mzs <- seq_along(ints)
#' peaks <- which(localMaxima(ints, hws = 3))
#' cols <- seq_along(peaks) + 1
#'
#' plot(mzs, ints, type = "h", ylim = c(0, 16))
#' points(mzs[peaks], ints[peaks], col = cols, pch = 20)
#'
#' v <- valleys(ints, peaks)
#' segments(mzs[v[, "left"]], 0, mzs[v[, "right"]], col = cols, lwd = 2)
#'
#' ## Known limitations for plateaus
#' y <- c(0, 0, 0, 0, 0, 1:5, 4:1, 0)
#' valleys(y, 10L) # left should be 5 here but is 1
#'
#' ## a possible workaround that may cause other problems
#' y[min(y) == y] <- Inf
#' valleys(y, 10L)
valleys <- function(x, p) {
    if (!is.numeric(x))
        stop("'x' has to be numeric vector.")
    if (!is.integer(p))
        stop("'p' has to be an integer vector.")

    ## extend x on both sides to ensure that local minima at both extremes are
    ## found
    x <- c(Inf, x, Inf)
    p <- p + 1L

    v <- which(localMaxima(-x, hws = 1L))
    ## local minima on the left of (before) the peaks
    l <- v[findInterval(p, v)]
    ## local minima on the right of (after) the peaks
    ## could use rev here but that would cause dispatching and length checking
    rv <- v[length(v):1L]
    np <- length(p)
    r <- rv[findInterval(-p[np:1L], -rv)[np:1L]]

    m <- matrix(c(l, p, r), ncol = 3L,
                dimnames = list(c(), c("left", "centroid", "right")))
    m - 1L
}
