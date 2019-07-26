#' @title Relaxed Value Matching
#'
#' @description
#'
#' These functions offer relaxed matching of one vector in another.
#' In contrast to the similar [`match()`] and [`%in%`] functions they
#' just accept `numeric` arguments but have an additional `tolerance`
#' argument that allows relaxed matching.
#'
#' `groupRun` groups consecutive values in `x` to a *run* if their difference
#' is smaller than defined by parameters `tolerance` and `ppm`. The function
#' thus evaluates the differences between consecutive values and if they are
#' smaller than `< tolerance + ppm(x, ppm)` they are grouped into the same
#' run. In other words, if `x[3] - x[2]` as well as `x[4] - x[3]` is
#' `< tolerance + ppm(x, ppm)` they are all grouped into the same run. Note
#' that `x` is expected to be increasingly ordered.
#'
#' `joinNumeric`: joins two `numeric` vectors by mapping values in `x` with
#' values in `y` and *vice versa* if they are similar enough (provided the
#' `tolerance` and `ppm` specified). The function returns a list with the
#' indices of mapped values in `x` and `y`. Parameter `join` allows to define
#' how the vectors will be joined: `join = "left"`: values in `x` will be
#' mapped to values in `y`, elements in `y` not matching any value in `x` will
#' be discarded. `join = "right"`: same as `join = "left"` but for `y`.
#' `join = "outer"`: return matches for all values in `x` and in `y`.
#' `join = "inner"`: report only indices of values that could be mapped.
#' Note that at present the function will only report one index for multiple
#' matches (e.g. if one value in `x` could be mapped to more than one value
#' in `y`). A warning is displayed if multiple matches were found.
#' See below for examples.
#'
#' @param duplicates `character(1)`, how to handle duplicated matches.
#'
#' @param join For `joinNumeric`: `character` defining the way how vectors
#'     should be joined. Allowed are `"outer"`, `"left"`, `"right"` and
#'     `"inner"`. See function description for details.
#'
#' @param nomatch `numeric(1)`, if the difference
#'     between the value in `x` and `table` is larger than
#'     `tolerance` `nomatch` is returned.
#'
#' @param ppm For `groupRun`: `numeric(1)` representing a relative,
#'     value-specific parts-per-million (PPM) tolerance.
#'
#' @param table `numeric`, the values to be matched against. In contrast to
#'     [`match()`] `table` has to be sorted in increasing order.
#'
#' @param tolerance `numeric`, accepted tolerance. Could be of length one or
#'     the same length as `table`.
#'
#' @param x `numeric`, the values to be matched.
#'
#' @param y For `joinNumeric`: `numeric` to match values in `x` against.
#'
#' @details
#' The `tolerance` argument could be set to `0` to get the same results as for
#' [`match()`]. If it is set to `Inf` (default) the index of the closest values
#' is returned without any restriction.
#'
#' It is not guaranteed that there is a one-to-one matching for neither the
#' `x` to `table` nor the `table` to `x` matching.
#'
#' If multiple elements in `x` match a single element in `table` all their
#' corresponding indices are returned if `duplicates="keep"` is set (default).
#' This behaviour is identical to [`match()`]. For `duplicates="closest"` just
#' the closest element in `x` gets the corresponding index in `table` and
#' for `duplicates="remove"` all elements in `x` that match to the same element
#' in `table` are set to `nomatch`.
#'
#' If a single element in `x` matches multiple elements in `table` the *closest*
#' is returned for `duplicates="keep"` or `duplicates="duplicates"` (*keeping*
#' multiple matches isn't possible in this case because the implementation relies
#' on [`findInterval`]). If the differences between `x` and the corresponding
#' matches in `table` are identical the lower index (the smaller element
#' in `table`) is returned. For `duplicates="remove"` all multiple matches
#' are returned as `nomatch` as above.
#'
#' @return `closest` returns an `integer` vector of the same length as `x`
#' giving the closest position in `table` of the first match or `nomatch` if
#' there is no match.
#'
#' @rdname matching
#' @author Sebastian Gibb, Johannes Rainer
#' @seealso [`match()`]
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
#' closest(x, y, tolerance = ppm(y, 20))
#'
#' ## Same using 50 ppm
#' closest(x, y, tolerance = ppm(y, 50))
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

    if (!all(is.numeric(tolerance)) || any(tolerance < 0))
        stop("'tolerance' has to be a 'numeric' larger or equal zero.")

    if (!is.numeric(nomatch) || length(nomatch) != 1L)
        stop("'nomatch' has to be a 'numeric' of length one.")

    ntable <- length(table)
    tolerance <- rep_len(tolerance, ntable) + sqrt(.Machine$double.eps)
    duplicates <- match.arg(duplicates)

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
#' ## Are there any common values?
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

#' @rdname matching
#'
#' @return `groupRun` returns an `integer` indicating which elements in `x` are
#'     close enough to be considered the *same* (group).
#'
#' @export
#'
#' @examples
#'
#' ## Group the input values
#' x <- seq(1, 4, 0.1)
#'
#' ## Each element is it's own group
#' groupRun(x)
#'
#' ## With tolerance of 0.1 all are assigned to the same group (i.e. difference
#' ## between consecutive elements is <= tolerance.
#' groupRun(x, tolerance = 0.1)
#'
#' x <- 1:20
#' x[20] <- x[19] + ppm(x[19], 20)
#' groupRun(x)
#' groupRun(x, ppm = 20)
groupRun <- function(x, tolerance = 0, ppm = 0) {
    tolerance <- tolerance + sqrt(.Machine$double.eps)
    if (ppm > 0)
        cumsum(c(0L, diff(x) > (tolerance + ppm(x[-length(x)], ppm)))) + 1L
    else
        cumsum(c(0L, diff(x) > tolerance)) + 1L
}

#' @rdname matching
#'
#' @return
#'
#' `joinNumeric` returns a `list` with elements `x` and `y`, each representing
#' the index of the values in `x` matching the corresponding value in `y` (or
#' `NA` if the value does not match).
#'
#' @export
#'
#' @examples
#'
#' ## Examples for joinOrderedNumeric
#' x <- c(1, 2, 3, 6)
#' y <- c(3, 4, 5, 6, 7)
#'
#' ## Outer joind of the two vectors
#' res <- joinNumeric(x, y)
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' ## Left join: keep all from x, only those from y that match a peak in x
#' res <- joinNumeric(x, y, join = "left")
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' ## If there are multiple matches, the **last** is reported
#' y <- c(3, 4, 5, 6, 6, 7)
#'
#' res <- joinNumeric(x, y)
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' ## Right join
#' res <- joinNumeric(x, y, join = "right")
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' ## Inner join
#' res <- joinNumeric(x, y, join = "inner")
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' ## More real case scenario: ppm difference.
#' x <- c(23.4, 34.3, 123.5, 213.6, 412.6)
#' y <- c(12.2, 23.5, 213.6 + ppm(213.6, 5), 237, 412.5)
#'
#' res <- joinNumeric(x, y)
#' res
#'
#' res <- joinNumeric(x, y, tolerance = 0.1, join = "outer")
#' res
#'
#' x[res$x]
#' y[res$y]
#'
#' res <- joinNumeric(x, y, ppm = 5, join = "inner")
#' res <- joinNumeric(x, rev(y), ppm = 5, join = "outer")
#' res <- joinNumeric(y, rev(y), ppm = 5, join = "outer")
#'
#' x[res$x]
#' y[res$y]
joinNumeric <- function(x, y, tolerance = 0, ppm = 0,
                               join = c("outer", "left", "right", "inner")) {
    join <- match.arg(join)
    vals <- sort(c(x, y))
    ## Multi-matches, will always take the **last**
    grps <- groupRun(vals, tolerance = tolerance, ppm = ppm)
    if (any(rle(grps)$lengths > 2))
        warning("Multiple matches between elements in 'x' and 'y'")
    x_idx <- y_idx <- rep(NA_integer_, grps[length(grps)])
    x_idx[grps[match(x, vals)]] <- seq_along(x)
    y_idx[grps[match(y, vals)]] <- seq_along(y)
    ## WARNING if multiple matches
    keep <- switch(join,
                   "outer" = rep(TRUE, length(x_idx)),
                   "left" = !is.na(x_idx),
                   "right" = !is.na(y_idx),
                   "inner" = !(is.na(y_idx) | is.na(x_idx)))
    list(x = x_idx[keep], y = y_idx[keep])
}
