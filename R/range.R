#' @title Range helper functions
#'
#' @description
#' These functions help to work with `numeric` ranges.
#'
#' @param x `numeric`, input values.
#' @param range `numeric(2)`, range to compare against.
#'
#' @author Sebastian Gibb
#' @export
#' @rdname range
#' @return `logical` vector of length `length(x)`.
#' @aliases between
#' @family helper functions for developers
#' @useDynLib MsCoreUtils, .registration = TRUE
#' @export
#' @examples
#' between(1:4, 2:3)
between <- function(x, range).Call(C_between, x, range)

#' @rdname range
#'
#' @export
#' @examples
#' 1:4 %between% 2:3
"%between%" <- between
