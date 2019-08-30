#' @title General utility functions
#'
#' @description
#'
#' - `asInteger`: convert `x` to an `integer` and throw an error if `x` is not
#'   a `numeric`.
#'
#' - `rbindFill`: combines instances of `matrix`, `data.frame` or `DataFrame`
#'   objects into a single instance adding eventually missing columns (filling
#'   them with `NA`s).
#'
#' @note
#'
#' `rbindFill` might not work if one of the columns contains S4classes.
#'
#' @param x input argument.
#'
#' @param ... For `rbindFill`: 2 or more: `matrix`, `data.frame` or `DataFrame`.
#'
#' @author Johannes Rainer, Sebastian Gibb
#'
#' @name utils
#'
#' @examples
#'
#' ## Convert numeric to integer
#' asInteger(3.4)
#'
#' asInteger(3)
#'
#' ## Combine matrices
#' a <- matrix(1:9, nrow = 3, ncol = 3)
#' colnames(a) <- c("a", "b", "c")
#' b <- matrix(1:12, nrow = 3, ncol = 4)
#' colnames(b) <- c("b", "a", "d", "e")
#' rbindFill(a, b)
#' rbindFill(b, a, b)
NULL

#' @rdname utils
#'
#' @export
asInteger <- function(x) {
    if (is.numeric(x))
        as.integer(x)
    else {
        arg <- deparse(substitute(x))
        stop("Argument ", arg, " should be a numeric or integer", call. = FALSE)
    }
}

#' @title Check for valid Window Size
#'
#' @param w `integer(1)`, window size
#' @param n `integer(1)`, length of the vector over which the window should be
#' applied
#' @return `TRUE` if valid otherwise throws an error
#' @noRd
.validateWindow <- function(w, n) {
    if (length(w) != 1L || !is.integer(w))
        stop("window has to be an integer of length 1.")
    if (w < 0L)
        stop("window has to be larger than zero.")
    if (w > n)
        stop("window has to be smaller than or equal to n.")
    TRUE
}
