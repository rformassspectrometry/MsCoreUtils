#' @title Which is the first/last TRUE value.
#'
#' @description
#' Determines the location, i.e., index of the first or last `TRUE` value in a
#' logical vector.
#'
#' @param x `logical`, vector.
#' @return `integer`, index of the first/last `TRUE` value. `integer(0)` if
#' no `TRUE` (everything `FALSE` or `NA`) was found.
#'
#' @rdname which
#' @author Sebastian Gibb
#' @seealso [`which.min()`]
#' @useDynLib MsCoreUtils, .registration = TRUE
#' @export
#' @examples
#' l <- 2 <= 1:3
#' which.first(l)
which.first <- function(x).Call("C_which_first", x)

#' @rdname which
#' @export
#' @examples
#' which.last(l)
which.last <- function(x).Call("C_which_last", x)
