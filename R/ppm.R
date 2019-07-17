#' PPM - Parts per Million
#'
#' `ppm` is a small helper function to mulitply *mz* values with a given *ppm*.
#'
#' @param x `numeric`, mz values.
#' @param ppm `numeric`, parts per million (ppm) values.
#'
#' @return `numeric`: `x * 1e-6`
#'
#' @author Sebastian Gibb
#' @export
#' @examples
#' ppm(c(1000, 2000), 5)
ppm <- function(x, ppm)x * ppm * 1e-6
