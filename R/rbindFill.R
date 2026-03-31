#' @title Combine R Objects by Row
#'
#' @description
#'
#' This function combines instances of `matrix`, `data.frame` or `DataFrame`
#' objects into a single instance adding eventually missing columns (filling
#' them with `NA`s).
#'
#' @param ... 2 or more: `matrix`, `data.frame` or `DataFrame`.
#'
#' @return Depending on the input a single `matrix`, `data.frame` or
#'     `DataFrame`.
#'
#' @note
#'
#' `rbindFill()` might not work if one of the columns contains S4 classes.
#'
#'
#' @author Johannes Rainer, Sebastian Gibb
#'
#' @family helper functions for developers
#'
#' @importMethodsFrom S4Vectors cbind nrow rownames colnames
#'
#' @importFrom methods as
#'
#' @export
#'
#' @examples
#' ## Combine matrices
#' a <- matrix(1:9, nrow = 3, ncol = 3)
#' colnames(a) <- c("a", "b", "c")
#' b <- matrix(1:12, nrow = 3, ncol = 4)
#' colnames(b) <- c("b", "a", "d", "e")
#' rbindFill(a, b)
#' rbindFill(b, a, b)
#'
#' ## Combine data.frame
#' d <- data.frame(a = 1:4, z = rep(TRUE, 4))
#' g <- data.frame(b = 1:3, z = rep(FALSE, 3))
#' rbindFill(d, g)
#'
#' ## Combine matrix and data.frames
#' res <- rbindFill(a, d)
#' res
#' class(res)
rbindFill <- function(...) {
    l <- list(...)

    if (length(l) == 1L && is.list(l[[1L]]))
        l <- l[[1L]]

    cnms <- c("matrix", "data.frame", "DataFrame", "DFrame")

    if (inherits(l, cnms))  # just one single object given as input, do nothing
        return(l)

    cls <- vapply(l, inherits, integer(length(cnms)), what = cnms, which = TRUE)
    rownames(cls) <- cnms

    if (any(!as.logical(colSums(cls))))
        stop("'rbindFill' just works for ", paste(cls, collapse = ", "))

    ## convert matrix to data.frame for easier and equal subsetting and class
    ## determination
    isMatrix <- as.logical(cls["matrix",])
    ## if (all(isMatrix))
    ##     return(.rbind_fill_matrix(l))
    l[isMatrix] <- lapply(l[isMatrix], as.data.frame)

    allcl <- unlist(
        lapply(l, function(ll) {
            vapply1c(ll, function(lll)class(lll)[1L], USE.NAMES = TRUE)
        })
    )
    allnms <- unique(names(allcl))
    allcl <- allcl[allnms]

    for (i in seq_along(l)) {
        diffcn <- setdiff(allnms, names(l[[i]]))
        if (length(diffcn))
            l[[i]][, diffcn] <- lapply(allcl[diffcn], as, object = NA)
    }
    r <- do.call(rbind, l)

    ## if we had just matrices as input we need to convert our temporary
    ## data.frame back to a matrix
    if (all(isMatrix))
        r <- as.matrix(r)
    r
}

## helper function to allow lapply(..., as, ...) in rbindFill
setAs("logical", "factor", function(from, to) factor(from))

#' @description
#'
#' Combines provided matrices into a single matrix eventually adding missing
#' columns (filled with `NA`). After adding missing columns, the matrices
#' are combined with `base::rbind` hence eventually changing the type of the
#' matrices.
#'
#' @param l `list` of `matrix`
#'
#' @return `matrix` with combined matrices from `l`
#'
#' @author Johannes Rainer
#'
#' @noRd
.rbind_fill_matrix <- function(l) {
    cnames <- unique(unlist(lapply(l, colnames)))
    do.call(rbind, lapply(l, function(z) {
        cnz <- colnames(z)
        if (length(cnz) != ncol(z))
            stop("All matrices need to have column names", call. = FALSE)
        miss <- setdiff(cnames, cnz)
        if (length(miss)) {
            z <- cbind(z, matrix(NA, ncol = length(miss),
                                 nrow = nrow(z)))
            colnames(z) <- c(cnz, miss)
        }
        if (!identical(unname(colnames(z)), unname(cnames)))
            z <- z[, cnames]
        z
    }))
}
