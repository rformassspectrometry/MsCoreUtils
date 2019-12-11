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
