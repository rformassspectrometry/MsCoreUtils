#' @title Refine Peak Centroids
#'
#' @description
#' This function refines the centroided values of a peak by weighting the y
#' values in the neighbourhood that belong most likely to the same peak.
#'
#' @param x `numeric`, i.e. m/z values.
#' @param y `numeric`, i.e. intensity values.
#' @param p `integer`, indices of identified peaks/local maxima.
#' @param k `integer(1)`, number of values left and right of the peak that
#'  should be considered in the weighted mean calculation.
#' @param threshold `double(1)`, proportion of the maximal peak intensity.
#'  Just values above are used for the weighted mean calclulation.
#' @param descending `logical`, if `TRUE` just values between the nearest
#'  valleys around the peak centroids are used.
#'
#' @details
#' For `descending = FALSE` the function looks for the `k` nearest neighbouring
#' data points and use their `x` for weighted mean with their corresponding `y`
#' values as weights for calculation of the new peak centroid. If `k` are chosen
#' too large it could result in skewed peak centroids, see example below.
#' If `descending = TRUE` is used the `k` should be general larger because it is
#' trimmed automatically to the nearest valleys on both sides of the peak so the
#' problem with skewed centroids is rare.
#'
#' @author Sebastian Gibb, Johannes Rainer
#' @export
#' @examples
#' ints <- c(5, 8, 12, 7, 4, 9, 15, 16, 11, 8, 3, 2, 3, 9, 12, 14, 13, 8, 3)
#' mzs <- seq_along(ints)
#'
#' plot(mzs, ints, type = "h")
#'
#' pidx <- as.integer(c(3, 8, 16))
#' points(mzs[pidx], ints[pidx], pch = 16)
#'
#' ## Use the weighted average considering the adjacent mz
#' mzs1 <- refineCentroids(mzs, ints, pidx,
#'                         k = 2L, descending = FALSE, threshold = 0)
#' mzs2 <- refineCentroids(mzs, ints, pidx,
#'                         k = 5L, descending = FALSE, threshold = 0)
#' mzs3 <- refineCentroids(mzs, ints, pidx,
#'                         k = 5L, descending = TRUE, threshold = 0)
#' points(mzs1, ints[pidx], col = "red", type = "h")
#' ## please recognize the artificial moved centroids of the first peak caused
#' ## by a too large k, here
#' points(mzs2, ints[pidx], col = "blue", type = "h")
#' points(mzs3, ints[pidx], col = "green", type = "h")
#' legend("topright",
#'        legend = paste0("k = ", c(2, 5, 5),
#'                        ", descending =", c("FALSE", "FALSE", "TRUE")),
#'        col = c("red", "blue", "green"), lwd = 1)
refineCentroids <- function(x, y, p, k = 2L, threshold = 0.33,
                            descending = FALSE) {
    if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y))
        stop("'x' and 'y' have to be numeric vectors of the same length.")

    if (missing(p) || !length(p))
        return(x)
    if (!is.integer(p))
        stop("'p' has to be an integer vector.")

    if (length(k) != 1L || !is.integer(k) || k < 0L)
        stop("'k' has to be an integer of length 1 and >= 0.")

    if (length(threshold) != 1L || !is.numeric(threshold) ||
        0 > threshold || threshold > 1)
        stop("'threshold' has to be a numeric between 0 and 1.")

    if (length(descending) != 1L || !is.logical(descending) ||
        is.na(descending))
        stop("'descending' has to be 'TRUE' or 'FALSE'.")

    k2 <- 2 * k + 1L
    i <- seq_len(k2) - 1L + rep(p, each = k2)

    if (descending)
        mask <- .peakRegionMask(y, p, k = k)
    else
        mask <- 1L

    threshold <- y[p] * threshold

    ## add elements to the left/right to avoid out-of-boundary errors
    side <- rep.int(0L, k)
    x <- c(side, x, side)[i]
    y <- c(side, y, side)[i]

    dim(x) <- dim(y) <- c(k2, length(p))

    y <- y * mask * t(t(y) > threshold)

    ## weighted average
    colSums(x * y) / colSums(y)
}

#' @title Peak Region Mask
#'
#' @description
#' This function finds the mz region spanning by a peak. It creates an 0/1
#' matrix used for multiplications in other functions.
#'
#' @param x `numeric`, e.g. intensity values.
#'
#' @param p `integer`, indices of identified peaks/local maxima.
#'
#' @param k `integer(1)`: maximum number of values left and right of the
#'     peak that should be looked for valleys.
#'
#' @return A `matrix` with a column for each peak in `p` and `2 * k + 1`
#' rows where the middle row `k + 1` is the peak centroid. If the values is `1`
#' the index belongs to the peak region.
#'
#' @author Sebastian Gibb
#'
#' @noRd
#'
#' @examples
#' ints <- c(5, 8, 12, 7, 4, 9, 15, 16, 11, 8, 3, 2, 3, 2, 9, 12, 14, 13, 8, 3)
#' mzs <- seq_along(ints)
#' peaks <- which(localMaxima(ints, hws = 3L))
#'
#' m <- MsCoreUtils:::.peakRegionMask(ints, peaks, k = 5L)
.peakRegionMask <- function(x, p, k = 30L) {
    v <- valleys(x, p)

    ## if the valleys outside of the k window, set to k
    v <- abs(v - p)
    v[v > k] <- k

    ## valley to peak regions
    ## before/left = (k - l) x `0` => 1:(p - l - 1), region before peak
    ## pr/centroid = (l + r + 1 (peak) = x `1` => (p - l):(r - p), peak region
    ## after/right = (k + 1L - r) x `0` => (r - p + 1):(2 * k + 1), region after
    ## peak
    v[, "centroid"] <- v[, "left"] + v[, "right"] + 1L
    v[, c("left", "right")] <- k - v[, c("left", "right")]
    n <- length(p)
    m <- rep.int(rep.int(c(0L, 1L, 0L), n), t(v))
    dim(m) <- c(k * 2L + 1L, n)
    m
}
