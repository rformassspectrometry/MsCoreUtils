#' @title Summing MS Intensity Values
#'
#' @description
#'
#' `sumi` sums mass spectrometry intensity values, e.g. from a spectrum or
#' chromatogram. In contrast to the base R [sum()] function this function
#' returns `NA_real_` if all intensity values are `NA` or if `length(x)` is 0.
#'
#' @param x `numeric` with intensity values to be summed up. Will be coerced
#'     to `numeric` using `as.double`.
#'
#' @return `numeric(1)` representing the sum of values in `x`. Always returns
#'     a numeric (double) even if `x` is an integer.
#'
#' @author Johannes Rainer
#'
#' @seealso [maxi()]
#'
#' @export
#'
#' @examples
#'
#' x <- c(3.2, 34.4, 1.3, NA)
#' sumi(x)
#'
#' ## Compared to base R sum:
#' sum(x)
#' sum(x, na.rm = TRUE)
#'
#' sum(numeric(), na.rm = TRUE)
#' sumi(numeric())
#'
#' sum(c(NA, NA), na.rm = TRUE)
#' sumi(c(NA, NA))
sumi <- function(x) {
    .Call(C_sumi, as.double(x))
}
