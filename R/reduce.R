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
#' @author Johannes Rainer
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
#' @export
reduce <- function(start = numeric(), end = numeric(), .check = TRUE) {
    l <- length(start)
    if (!l)
        return(list(numeric(), numeric()))
    if (.check) {
        if (l != length(end))
            stop("'start' and 'end' need to have the same length")
        if (!all(start <= end))
            stop("Values in 'start' have to be smaller or equal to",
                 "the respective values in 'end'")
    }
    idx <- order(start, method = "radix")
    start_res <- end_res <- rep(NA_real_, l)
    pos <- 1L
    start_res[pos] <- start[idx[1L]]
    end_res[pos] <- end[idx[1L]]
    for (i in idx[-1L]) {
        if (start[i] < end_res[pos]) {
            if (end[i] > end_res[pos])
                end_res[pos] <- end[i]
        } else {
            pos <- pos + 1L
            start_res[pos] <- start[i]
            end_res[pos] <- end[i]
        }
    }
    fin <- seq_len(pos)
    list(start_res[fin], end_res[fin])
}
