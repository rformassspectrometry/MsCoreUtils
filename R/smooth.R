#' @title Smoothing
#'
#' @description
#' This function smoothes a numeric vector.
#'
#' @param x `numeric`, i.e. m/z values.
#' @param hws `integer(1)`, half window size, the resulting window reaches from
#' `(i - hws):(i + hws)`.
#' @param method `character(1)`, used method. Currently MovingAverage,
#' WeightedMovingAverage and SavitzkyGolay are supported.
#' @param ... further arguments passed to `method`. Currently just `k` for
#' the *polynomial order* in `"SavitzkyGolay"` is supported.
#'
#' @return A `numeric` of the same length as `x`.
#'
#' @details
#' `method = "WeightedMovingAverage"`: calculates a weighted moving average
#' with weights depending on the distance from the center
#' calculated as `1/2^abs(-hws:hws)` with the sum of all weigths normalized
#' to 1.
#'
#' `method = "SavitzkyGolay"`: uses the Savitzky-Golay-Filter for smoothing. It
#' supports an additional argument `k` (`integer(1)`) that controls the order of
#' the polynomial (default: `k = 3`) used to calculated the coefficients.
#'
#' @author Sebastian Gibb, Sigurdur Smarason (weighted moving average)
#' @references
#' A. Savitzky and M. J. Golay. 1964.
#' Smoothing and differentiation of data by simplified least squares procedures.
#' Analytical chemistry, 36(8), 1627-1639.
#'
#' M. U. Bromba and H. Ziegler. 1981.
#' Application hints for Savitzky-Golay digital smoothing filters.
#' Analytical Chemistry, 53(11), 1583-1586.
#'
#' @aliases smooth
#' @family noise estimation and smoothing functions
#' @export
#' @examples
#' x <- c(1:10, 9:1)
#' plot(x, type = "b", pch = 20)
#' m  <- c("MovingAverage", "WeightedMovingAverage", "SavitzkyGolay")
#' for (i in seq_along(m)) {
#'     lines(smooth(x, hws = 3L, method = m[i]), col = i + 1L, pch = 20,
#'           type = "b")
#' }
#' legend("bottom", legend = c("x", m), pch = 20,
#'        col = c(1L, seq_along(m) + 1L))
smooth <- function(x, hws, method = c("MovingAverage", "WeightedMovingAverage",
                                      "SavitzkyGolay"), ...) {
    ft <- switch(match.arg(method),
                 "MovingAverage" = .coefMA(hws, ...),
                 "WeightedMovingAverage" = .coefWMA(hws, ...),
                 "SavitzkyGolay" = .coefSG(hws, ...))
    .filter(x = x, filter = ft)
}

#' Coefficients for Simple Moving Average
#'
#' @param hws `integer(1)`, half window size, the resulting window reaches from
#' `(i - hws):(i + hws)`.
#' @param ... ignored.
#' @return A `matrix` with filter coefficients.
#'
#' @noRd
.coefMA <- function(hws, ...) {
    w <- 2L * hws + 1L
    matrix(1L / w, nrow = w, ncol = w)
}

#' Coefficients for Moving Average with Weighting Distance from Center
#'
#' @param hws `integer(1)`, half window size, the resulting window reaches from
#' `(i - hws):(i + hws)`.
#' @param ... ignored.
#' @return A `matrix` with filter coefficients.
#'
#' @noRd
.coefWMA <- function(hws, ...) {
    w <- 2L * hws + 1L
    k <- 1 / 2^abs(-hws:hws)
    matrix(k / sum(k), nrow = w, ncol = w, byrow = TRUE)
}

#' Coefficients for Savitzky-Golay-Filter
#'
#' @param hws `integer(1)`, half window size, the resulting window reaches from
#' `(i - hws):(i + hws)`.
#' @param k `integer(1)`, polynomial order of the filter (`k = 0` == moving
#' average)
#' @param ... ignored.
#' @return A `matrix` with filter coefficients.
#'
#' @references
#' Savitzky, A., & Golay, M. J. (1964). Smoothing and differentiation of data
#' by simplified least squares procedures. Analytical chemistry, 36(8), 1627-1639.
#'
#' Implementation based on:
#' Steinier, J., Termonia, Y., & Deltour, J. (1972). Comments on Smoothing and
#' differentiation of data by simplified least square procedure.
#' Analytical Chemistry, 44(11), 1906-1909.
#'
#' Implemention of left/right extrema based on:
#' sgolay in signal 0.7-3/R/sgolay.R by Paul Kienzle <pkienzle@users.sf.net>
#' modified by Sebastian Gibb <mail@sebastiangibb.de>
#' @noRd
.coefSG <- function(hws, k = 3L, ...) {
    nk <- k + 1L
    k <- seq_len(nk) - 1L
    w <- 2L * hws + 1L

    if (w < nk)
        stop("The window size has to be larger than the polynomial order.")

    K <- matrix(k, nrow = w, ncol = nk, byrow = TRUE)

    ## filter is applied to -hws:hws around current data point
    ## to avoid removing (NA) of left/right extrema
    ## lhs: 0:(2 * hws)
    ## rhs: (n - 2 * hws):n

    ## filter matrix contains 2 * hws + 1 rows
    ## row 1:hws == lhs coef
    ## row hws + 1 == typical sg coef
    ## row (n - hws - 1):n == rhs coef
    F <- matrix(NA_real_, nrow = w, ncol = w)
    for (i in seq_len(hws + 1L)) {
        M <- matrix(seq_len(w) - i, nrow = w, ncol = nk, byrow = FALSE)
        X <- M^K
        F[i, ] <- (solve(t(X) %*% X) %*% t(X))[1L, ]
    }
    ## rhs (row (n-m):n) are equal to reversed lhs
    F[seq.int(to = w, length.out = hws), ] <- rev(F[seq_len(hws), ])
    F
}

#' Wrapper around stats::filter to remove time series attributes and
#' fix NA at left/right extrema
#'
#' @param x `numeric`, vector that should be smoothed.
#' @param filter `matrix`, result from `.coef*` functions.
#' @return A `numeric` of the same length as `x`.
#'
#' @importFrom stats filter
#' @noRd
.filter <- function(x, filter) {
    n <- length(x)
    w <- dim(filter)[1L]
    .validateWindow(w, n)
    hws <- trunc(w / 2L)
    y <- filter(x = x, filter = filter[hws + 1L, ], sides = 2L)
    attributes(y) <- NULL

    ## fix left/right extrema
    lhws <- seq_len(hws)
    y[lhws] <- filter[lhws, , drop = FALSE] %*% x[seq_len(w)]
    y[seq.int(to = n, length.out = hws)] <-
        filter[seq.int(to = w, length.out = hws), , drop = FALSE] %*%
        x[seq.int(to = n, length.out = w)]
    y
}
