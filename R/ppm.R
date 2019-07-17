#' PPM - Parts per Million
#'
#' `ppm` is a small helper function to mulitply *ppm* values with `1e-6`.
#'
#' @param x `numeric`, ppm values.
#'
#' @return `numeric`: `x * 1e-6`
#'
#' @author Sebastian Gibb
#' @export
#' @examples
#' ppm(5)
ppm <- function(x)x * 1e-6
