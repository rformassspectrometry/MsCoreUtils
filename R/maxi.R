#' @title Maximum MS Intensity Value
#'
#' @description
#'
#' `maxi` determines the maximum or mass spectrometry intensity values, e.g.
#' from a spectrum or chromatogram. In contrast to the base R [max()] function
#' this function returns `NA_real_` if all intensity values are `NA` or if
#' `length(x)` is 0 (the base R `max` function returns `-Inf` in these cases).
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
#' maxi(x)
#'
#' ## Compared to base R max:
#' max(x)
#' max(x, na.rm = TRUE)
#'
#' max(numeric(), na.rm = TRUE)
#' maxi(numeric())
#'
#' max(c(NA, NA), na.rm = TRUE)
#' maxi(c(NA, NA))
maxi <- function(x) {
    .Call(C_maxi, as.double(x))
}
