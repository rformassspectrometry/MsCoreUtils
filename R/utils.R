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
