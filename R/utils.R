#' @title Find all possible combinations
#'
#' @description
#' Similar to `expand.grid` but expects a `numeric` vector as input and returns
#' the indices.
#'
#' @param `x` `integer`, group numbers
#' @return `list`, each element represents a possible combination
#' @author Sebastian Gibb
#' @noRd
#' @examples
#' .combinations(c(1, 2, 2, 2, 3, 3))
.combinations <- function(x) {
    r <- rle(x)
    ncs <- cumsum(c(0L, r$lengths))
    ncmb <- prod(r$lengths)

    if (ncmb > 1e10)
        stop("too many possible combinations.")

    times <- 1L
    l <- vector(mode = "list", length = length(r))

    for (i in seq_along(r$lengths)) {
        n <- r$lengths[i]
        ncmb <- ncmb / n
        l[[i]] <- rep.int(rep.int(ncs[i] + seq_len(n), rep.int(times, n)), ncmb)
        times <- times * n
    }
    .transposeList(l)
}

#' @title Follower/Prev Identical
#'
#' @description
#' Tests whether the previous/following element in a vector is identical.
#'
#' @param x vector
#'
#' @return `logical`
#' @author Sebastian Gibb
#' @noRd
#' @examples
#' x <- c(1, 1, NA, 3, 4, 4, 5, 6, 6, 6, NA, 7, 8, 8)
#' .isFollowerIdentical(x)
#' .isPrecursorIdentical(x)
.isFollowerIdentical <- function(x) {
    x <- x[-1L] == x[-length(x)]
    c(x & !is.na(x), FALSE)
}
.isPrecursorIdentical <- function(x) {
    x <- x[-1L] == x[-length(x)]
    c(FALSE, x & !is.na(x))
}

#' @title Transpose List
#'
#' @description
#' Transpose a `n * m` `list` into an `m * n` one.
#'
#' @param x `list`
#' @return `list`
#' @author Sebastian Gibb
#' @noRd
#' @examples
#' .transposeList(list(a = 1:10, b = 11:20, c = 21:30))
.transposeList <- function(x) {
    n <- unique(lengths(x))

    if (!is.list(x) || length(n) != 1L)
        stop("'e' has to be a list with elements of equal length.")
    l <- split(unlist(x, use.names = FALSE), seq_len(n))
    names(l) <- NULL
    l
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
