#' @title Reduce overlapping numeric ranges to disjoined ranges
#'
#' @description
#'
#' The `reduce()` function *reduces* the provided numeric ranges to
#' non-overlapping (disjoint) ranges. This is similar to the
#' `IRanges::reduce()` function, but works with `numeric` vectors instead of
#' integer ranges (`IRanges`).
#'
#' @note
#'
#' The *IRanges* package defines a `reduce()` method for `IRanges` and other
#' S4 classes. This `reduce()` function is not an S4 method, but a function,
#' thus it is suggested to specifically import it if used in another R package,
#' or to call it with `MsCoreUtils::reduce()`.
#'
#' @param start `numeric` with the lower (start) values for each numeric range.
#'
#' @param end `numeric` with the upper (end) values for each numeric range.
#'     Has to match the length of `start` and `all(start <= end)` has to be
#'     `TRUE`.
#'
#' @param .check `logical(1)` whether input parameter checks should be
#'     performed.
#'
#' @return `list` of length 2, the first element being the start (mininum)
#'     values for the disjoint ranges, the second the end (maximum) values.
#'
#' @author Johannes Rainer and Sebastian Gibb
#'
#' @examples
#'
#' ## Define start and end values for the numeric ranges
#' s <- c(12.23, 21.2, 13.4, 14.2, 15.0, 43.12)
#' e <- c(12.40, 24.1, 14.4, 16.2, 15.2, 55.23)
#'
#' reduce(s, e)
#'
#' ## Empty vectors
#' reduce()
#'
#' ## Single value
#' reduce(3.12, 34)
#'
#' ## Non-overlapping ranges
#' reduce(c(3, 9), c(4, 19))
#' @useDynLib MsCoreUtils, .registration = TRUE
#' @export
reduce <- function(start = numeric(), end = numeric(), .check = TRUE)
    .Call(C_reduce, as.numeric(start), as.numeric(end), .check)
