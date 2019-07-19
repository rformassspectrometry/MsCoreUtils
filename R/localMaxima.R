#' Local Maxima
#'
#' This function finds local maxima in a numeric vector. A local maximum is
#' defined as maximum in a window of the current index +/- `hws`.
#'
#' @param x `numeric`, vector that should be searched for local maxima.
#' @param hws `numeric`, half window size, the resulting window reaches from
#' `(i - hws):(i + hws)`.
#'
#' @return A `logical` of the same length as `x` that is `TRUE` for each local
#' maxima.
#' @author Sebastian Gibb
#' @useDynLib MsCoreUtils
#' @export
#' @examples
#' x <- c(1:5, 4:1, 1:10, 9:1, 1:5, 4:1)
#' localMaxima(x)
#' localMaxima(x, hws = 10)
localMaxima <- function(x, hws = 1L) {
    i <- seq_along(x) + hws
    side <- rep.int(0L, hws)
    .Call("C_localMaxima", c(side, x, side), hws)[i]
}
