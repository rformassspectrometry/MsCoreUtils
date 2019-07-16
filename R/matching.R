#' Relaxed Value Matching
#'
#' These functions offer relaxed matching of one vector in another.
#' In contrast to the similar [`match`] and [`%in%`] functions they
#' just accept `numeric` arguments but have an additional `tolerance`
#' argument that allows relaxed matching.
#'
#' @param x `numeric`, the values to be matched.
#' @param table `numeric`, the values to be matched against. In contrast to
#' [`match`] `table` has to be sorted in increasing order.
#' @param tolerance `numeric`, accepted tolerance. Use `0` to get the
#' same behaviour as [`match`]. Use `Inf` (default) to match to the closest
#' value without restrictions. Could be of length one or the same length as
#' `table`.
#' @param duplicates `character(1)`, it is possible that many `x` match one
#' element in `table`. For `duplicates="keep"` (default) all of them are
#' returned. For `duplicates="closest"` just the `x` that is closest to the
#' element in `table` gets an index (all the other will get `nomatch`). If the
#' difference is exactly identical the first one in `x` will get an index. For
#' `duplicates="remove"` all duplicated matches are returned as `nomatch`. If
#' a single element in `x` matches to multiple elements in `table` the *closest*
#' is returned for `duplicates="keep"` or `duplicates="duplicates"` (*keeping*
#' multiple matches isn't possible in this case because the implentation relies
#' on [`findInterval`]). If the difference is identical the index of the
#' smaller element in `table` is returned.
#' For `duplicates="remove"` all multiple matches are returned as `nomatch`.
#' @param nomatch `numeric(1)`, if the difference
#' between the value in `x` and `table` is larger than
#' `tolerance` `nomatch` is returned.
#'
#' @return `closest` returns an `integer` vector of the same length as `x`
#' giving the closest position in `table` of the first match or `nomatch` if
#' there is no match.
#'
#' @rdname matching
#' @author Sebastian Gibb
#' @seealso [`match`]
#' @aliases closest
#' @export
#' @examples
#' ## Define two vectors to match
#' x <- c(1, 3, 5)
#' y <- 1:10
#'
#' ## Compare match and closest
#' match(x, y)
#' closest(x, y)
#'
#' ## If there is no exact match
#' x <- x + 0.1
#' match(x, y) # no match
#' closest(x, y)
#'
#' ## Some new values
#' x <- c(1.11, 45.02, 556.45)
#' y <- c(3.01, 34.12, 45.021, 46.1, 556.449)
#'
#' ## Using a single tolerance value
#' closest(x, y, tolerance = 0.01)
#'
#' ## Using a value-specific tolerance accepting differences of 20 ppm
#' closest(x, y, tolerance = x * ppm(20))
#'
#' ## Same using 50 ppm
#' closest(x, y, tolerance = x * ppm(50))
#'
#' ## Sometimes multiple elements in `x` match to `table`
#' x <- c(1.6, 1.75, 1.8)
#' y <- 1:2
#' closest(x, y, tolerance = 0.5)
#' closest(x, y, tolerance = 0.5, duplicates = "closest")
#' closest(x, y, tolerance = 0.5, duplicates = "remove")
closest <- function(x, table, tolerance = Inf,
                    duplicates = c("keep", "closest", "remove"),
                    nomatch = NA_integer_) {

    if (!is.numeric(tolerance) || tolerance < 0)
        stop("'tolerance' has to be a 'numeric' larger or equal zero.")

    if (!is.numeric(nomatch) || length(nomatch) != 1L)
        stop("'nomatch' has to be a 'numeric' of length one.")

    tolerance <- rep_len(tolerance, length(table)) + sqrt(.Machine$double.eps)
    duplicates <- match.arg(duplicates)
    ntable <- length(table)

    lIdx <- findInterval(x, table, rightmost.closed = FALSE, all.inside = TRUE)
    rIdx <- lIdx + 1L

    lIdx[lIdx == 0L] <- 1L
    rIdx[rIdx > ntable] <- ntable

    lDiff <- abs(table[lIdx] - x)
    rDiff <- abs(table[rIdx] - x)

    lDiff[lDiff > tolerance[lIdx]] <- Inf
    rDiff[rDiff > tolerance[rIdx]] <- Inf

    d <- which(lDiff > rDiff)

    lIdx[d] <- rIdx[d]

    ## no match at all
    lIdx[is.infinite(lDiff) & is.infinite(rDiff)] <- nomatch

    ## duplicated matches
    if (duplicates == "remove") {
        lIdx[duplicated(lIdx) | duplicated(lIdx, fromLast = TRUE)] <- nomatch
        lIdx[is.finite(lDiff) & is.finite(rDiff)] <- nomatch
    } else if (duplicates == "closest") {
        ## lIdx could be updated
        o <- order(abs(table[lIdx] - x))
        m <- lIdx[o]
        m[duplicated(m)] <- nomatch
        lIdx[o] <- m
    }

    as.integer(lIdx)
}

#' @rdname matching
#'
#' @return `common` returns a `logical` vector of length `x` that is `TRUE` if the
#' element in `x` was found in `table`. It is similar to [`%in%`].
#' @aliases common
#' @seealso [`%in%`]
#' @export
#' @examples
#'
#' ## Are there common values?
#' x <- c(1.6, 1.75, 1.8)
#' y <- 1:2
#' common(x, y, tolerance = 0.5)
#' common(x, y, tolerance = 0.5, duplicates = "closest")
#' common(x, y, tolerance = 0.5, duplicates = "remove")
common <- function(x, table, tolerance = Inf,
                   duplicates = c("keep", "closest", "remove")) {
    closest(x, table, tolerance = tolerance, duplicates = duplicates,
            nomatch = 0L) > 0L
}
