#' @title Maximum MS Intensity Value
#'
#' @description
#'
#' `maxi` determines the maximum or mass spectrometry intensity values, e.g.
#' from a spectrum or chromatogram. In contrast to the base R [max()] function
#' this function returns `NA_real_` if all intensity values are `NA` or if
#' `length(x)` is 0 (the base R `max` function returns `-Inf` in these cases).
#'
#' @param x `numeric` with intensity values from which the maximum should be
#'     reported. Will be coerced to `numeric`.
#'
#' @return `numeric(1)` representing the maximum of values in `x`. Returns
#'     always a `numeric` (double) even if `x` is an integer.
#'
#' @note
#'
#' For larger `x` (length > 1000) this function becomes slower than a base R
#' implementation using `max`. For `x` consisting of just missing values
#' (which is common when ion chromatograms are extracted from MS spectral data)
#' `maxi` however outperforms `max`.
#'
#' @author Johannes Rainer, Sebastian Gibb
#'
#' @seealso [sumi()]
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
    .Call(C_maxi, x)
}
