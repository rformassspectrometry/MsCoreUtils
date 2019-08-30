#' @title Combine R Objects by Row
#'
#' @description
#' This function combines instances of `matrix`, `data.frame` or `DataFrame`
#' objects into a single instance adding eventually missing columns (filling
#' them with `NA`s).
#'
#' @param ... 2 or more: `matrix`, `data.frame` or `DataFrame`.
#'
#' @return Depending on the input a single `matrix`, `data.frame` or
#' `DataFrame`.
#'
#' @note
#' `rbindFill` might not work if one of the columns contains S4 classes.
#'
#'
#' @author Johannes Rainer, Sebastian Gibb
#'
#' @family helper functions for developers
#' @importMethodsFrom S4Vectors cbind nrow rownames colnames
#' @importFrom methods as
#' @export
#' @examples
#' ## Combine matrices
#' a <- matrix(1:9, nrow = 3, ncol = 3)
#' colnames(a) <- c("a", "b", "c")
#' b <- matrix(1:12, nrow = 3, ncol = 4)
#' colnames(b) <- c("b", "a", "d", "e")
#' rbindFill(a, b)
#' rbindFill(b, a, b)
rbindFill <- function(...) {
    l <- list(...)

    if (length(l) == 1L)
        l <- l[[1L]]

    cl <- vapply1c(l, class)

    stopifnot(all(cl %in% c("matrix", "data.frame", "DataFrame", "DFrame")))

    ## convert matrix to data.frame for easier and equal subsetting and class
    ## determination
    isMatrix <- cl == "matrix"
    l[isMatrix] <- lapply(l[isMatrix], as.data.frame)

    allcl <- unlist(lapply(l, vapply1c, class, USE.NAMES = TRUE))
    allnms <- unique(names(allcl))
    allcl <- allcl[allnms]

    for (i in seq(along=l)) {
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
