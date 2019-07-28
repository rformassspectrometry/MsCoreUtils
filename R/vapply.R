#' @title vapply wrappers
#'
#' @description
#' These functions are short wrappers around typical `vapply` calls for easier
#' development.
#'
#' @param X a vector (atomic or `list`).
#' @param FUN the `function` to be applied to each element of `X`.
#' @param ... optional arguments to `FUN`.
#' @param USE.NAMES `logical`, should the return value be named.
#'
#' @author Sebastian Gibb
#' @export
#' @rdname vapply
#' @return `vapply1c` returns a vector of `character`s of length `X`.
#' @aliases vapply1c
#' @family helper functions for developers
#' @export
vapply1c <- function(X, FUN, ..., USE.NAMES=FALSE) {
    vapply(X=X, FUN=FUN, FUN.VALUE=NA_character_, ..., USE.NAMES=USE.NAMES)
}

#' @return `vapply1d` returns a vector of `double`s of length `X`.
#' @rdname vapply
#' @aliases vapply1d
#' @export
#' @examples
#' l <- list(a=1:3, b=4:6)
#' vapply1d(l, sum)
vapply1d <- function(X, FUN, ..., USE.NAMES=FALSE) {
    vapply(X=X, FUN=FUN, FUN.VALUE=NA_real_, ..., USE.NAMES=USE.NAMES)
}

#' @rdname vapply
#' @return `vapply1l` returns a vector of `logical`s of length `X`.
#' @aliases vapply1l
#' @export
vapply1l <- function(X, FUN, ..., USE.NAMES=FALSE) {
    vapply(X=X, FUN=FUN, FUN.VALUE=NA, ..., USE.NAMES=USE.NAMES)
}
