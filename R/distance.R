#' Calibrate function (workhorse of normalise)
#'
#' @param x `numeric`.
#' @param offset `numeric`.
#' @param scaling `numeric`.
#'
#' @return `numeric`.
#' @noRd
#' @examples
#' .calibrate(1:3, offset = 2, scaling = 2)
.calibrate <- function(x, offset = 0L, scaling = 1L) {
    (x - offset) / scaling
}

#' Weighted X Y function
#'
#' @param x `numeric`, e.g. mz values.
#' @param y `numeric`, e.g. intensity values.
#' @param m `numeric`, weighting `x`.
#' @param n `numeric`, weighting `y`.
#'
#' @return `numeric`.
#'
#' @references
#' Stein, S. E., and Scott, D. R. (1994).
#' Optimization and testing of mass spectral library search algorithms for
#' compound identification.
#' Journal of the American Society for Mass Spectrometry, 5(9), 859-866.
#' \doi{10.1016/1044-0305(94)87009-8}.
#'
#' @noRd
#' @examples
#' .weightxy(1:3, 4:6)
.weightxy <- function(x, y, m = 0, n = 0.5) {
    x ^ m * y ^ n
}
