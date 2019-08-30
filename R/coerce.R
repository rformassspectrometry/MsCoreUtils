#' @title Coerce functions
#'
#' @description
#'
#' - `asInteger`: convert `x` to an `integer` and throw an error if `x` is not
#'   a `numeric`.
#'
#' @param x input argument.
#'
#' @author Johannes Rainer
#'
#' @family coerce functions
#' @name coerce
#' @aliases asInteger
#' @export
#' @examples
#' ## Convert numeric to integer
#' asInteger(3.4)
#'
#' asInteger(3)
asInteger <- function(x) {
    if (is.numeric(x))
        as.integer(x)
    else {
        arg <- deparse(substitute(x))
        stop("Argument ", arg, " should be a numeric or integer", call. = FALSE)
    }
}
