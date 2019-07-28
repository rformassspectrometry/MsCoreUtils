#' @title Noise Estimation
#'
#' @description
#' This functions estimate the noise in the data.
#'
#' @param x `numeric`, x values for noise estimation (e.g. *mz*)
#' @param y `numeric`, y values for noise estimation (e.g. intensity)
#' @param method `character(1)` used method. Currently MAD (median absolute
#' deviation) and Friedman's SuperSmoother are supported.
#' @param ... further arguments passed to `method`.
#'
#' @return A `numeric` of the same length as `x` with the estimated noise.
#' @author Sebastian Gibb
#' @aliases noise
#' @seealso [`stats::mad()`], [`stats::supsmu()`]
#' @importFrom stats mad supsmu
#' @family noise estimation and smoothing functions
#' @export
#' @examples
#' x <- 1:20
#' y <- c(1:10, 10:1)
#' noise(x, y)
#' noise(x, y, method = "SuperSmoother", span = 1 / 3)
noise <- function(x, y, method = c("MAD", "SuperSmoother"), ...) {
    switch(match.arg(method),
           "MAD" = rep.int(mad(y), length(x)),
           "SuperSmoother" = supsmu(x, y, ...)$y
    )
}
