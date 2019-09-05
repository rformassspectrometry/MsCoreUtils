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
#' @export
#' @examples
#' between(1:4, 2:3)
between <- function(x, range) {
    if (!is.numeric(x))
        stop("'x' has to be numeric.")
    if (!is.numeric(range) || length(range) != 2L)
        stop("'range' has to be a numeric of length 2.")
    if (range[1L] > range[2L])
        range[2L:1L] <- range[1L:2L]
    range[1L] <= x & x <= range[2L]
}

#' @rdname range
#'
#' @export
#' @examples
#' 1:4 %between% 2:3
"%between%" <- between
