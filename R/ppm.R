#' @title PPM - Parts per Million
#'
#' @description
#' `ppm` is a small helper function to determine the parts-per-million for a
#' user-provided value and ppm.
#'
#' @param x `numeric`, value(s) used for ppm calculation, e.g. mz value(s).
#' @param ppm `numeric`, parts-per-million (ppm) value(s).
#'
#' @return `numeric`: parts-per-million of `x`
#'
#' @author Sebastian Gibb
#' @family helper functions for users
#' @export
#' @examples
#' ppm(c(1000, 2000), 5)
ppm <- function(x, ppm)x * ppm * 1e-6
