#' @title Relaxed Value Matching
#'
#' @description
#' These functions offer relaxed matching of one vector in another.
#' In contrast to the similar [`match()`] and [`%in%`] functions they
#' just accept `numeric` arguments but have an additional `tolerance`
#' argument that allows relaxed matching.
#'
#' @param x `numeric`, the values to be matched.
#' @param table `numeric`, the values to be matched against. In contrast to
#' [`match()`] `table` has to be sorted in increasing order.
#' @param tolerance `numeric`, accepted tolerance. Could be of length one or
#' the same length as `table`.
#' @param ppm `numeric(1)` representing a relative, value-specific
#'  parts-per-million (PPM) tolerance that is added to `tolerance`.
#' @param duplicates `character(1)`, how to handle duplicated matches.
#' @param nomatch `numeric(1)`, if the difference
#' between the value in `x` and `table` is larger than
#' `tolerance` `nomatch` is returned.
#'
#' @details
#' For `closest`/`common` the `tolerance` argument could be set to `0` to get
#' the same results as for [`match()`]/[`%in%`]. If it is set to `Inf` (default)
#' the index of the closest values is returned without any restriction.
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
#' @note
#' `closest` will replace all `NA` values in `x` by `nomatch` (that is identical
#' to the behaviour of `match`).
#'
#' @return `closest` returns an `integer` vector of the same length as `x`
#' giving the closest position in `table` of the first match or `nomatch` if
#' there is no match.
#'
#' @rdname matching
#' @author Sebastian Gibb
#' @seealso [`match()`]
#' @aliases closest
#' @family grouping/matching functions
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
closest <- function(x, table, tolerance = Inf, ppm = 0,
                    duplicates = c("keep", "closest", "remove"),
                    nomatch = NA_integer_) {

    if (!all(is.numeric(tolerance)) || any(tolerance < 0))
        stop("'tolerance' has to be a 'numeric' larger or equal zero.")

    if(!all(is.numeric(ppm)) || any(ppm < 0))
        stop("'ppm' has to be a 'numeric' larger or equal zero.")

    if (!is.numeric(nomatch) || length(nomatch) != 1L)
        stop("'nomatch' has to be a 'numeric' of length one.")

    table <- table[!is.na(table)]
    ntable <- length(table)
    if (!ntable)
        return(rep_len(nomatch, length(x)))

    tolerance <- rep_len(tolerance, ntable) + ppm(table, ppm) +
        sqrt(.Machine$double.eps)
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
    lIdx[is.infinite(lDiff) & is.infinite(rDiff)] <- NA_integer_

    ## duplicated matches
    if (duplicates == "remove") {
        lIdx[duplicated(lIdx) | duplicated(lIdx, fromLast = TRUE)] <-
            NA_integer_
        lIdx[is.finite(lDiff) & is.finite(rDiff)] <- NA_integer_
    } else if (duplicates == "closest") {
        ## lIdx could be updated
        o <- order(abs(table[lIdx] - x))
        m <- lIdx[o]
        m[duplicated(m)] <- NA_integer_
        lIdx[o] <- m
    }

    if (!identical(nomatch, NA_integer_))
        lIdx[is.na(lIdx)] <- nomatch

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
common <- function(x, table, tolerance = Inf, ppm = 0,
                   duplicates = c("keep", "closest", "remove")) {
    !is.na(closest(x, table, tolerance = tolerance, ppm = ppm,
                   duplicates = duplicates))
}

#' @rdname matching
#'
#' @details
#' `join`: joins two `numeric` vectors by mapping values in `x` with
#' values in `y` and *vice versa* if they are similar enough (provided the
#' `tolerance` and `ppm` specified). The function returns a `matrix` with the
#' indices of mapped values in `x` and `y`. Parameter `type` allows to define
#' how the vectors will be joined: `type = "left"`: values in `x` will be
#' mapped to values in `y`, elements in `y` not matching any value in `x` will
#' be discarded. `type = "right"`: same as `type = "left"` but for `y`.
#' `type = "outer"`: return matches for all values in `x` and in `y`.
#' `type = "inner"`: report only indices of values that could be mapped.
#'
#' @param y `numeric`, the values to be joined. Should be sorted.
#' @param type `character(1)`, defines how `x` and `y` should be joined. See
#' details for `join`.
#'
#' @note `join` is based on `closest(x, y, tolerance, duplicates = "closest")`.
#' That means for multiple matches just the closest one is reported.
#'
#' @return `join` returns a `matrix` with two columns, namely `x` and `y`,
#' representing the index of the values in `x` matching the corresponding value
#' in `y` (or `NA` if the value does not match).
#'
#' @export
#' @examples
#'
#' ## Join two vectors
#' x <- c(1, 2, 3, 6)
#' y <- c(3, 4, 5, 6, 7)
#'
#' jo <- join(x, y, type = "outer")
#' jo
#' x[jo$x]
#' y[jo$y]
#'
#' jl <- join(x, y, type = "left")
#' jl
#' x[jl$x]
#' y[jl$y]
#'
#' jr <- join(x, y, type = "right")
#' jr
#' x[jr$x]
#' y[jr$y]
#'
#' ji <- join(x, y, type = "inner")
#' ji
#' x[ji$x]
#' y[ji$y]
join <- function(x, y, tolerance = 0, ppm = 0,
                 type = c("outer", "left", "right", "inner")) {
    switch(match.arg(type),
           "outer" = .joinOuter(x, y, tolerance = tolerance, ppm = ppm),
           "left" = .joinLeft(x, y, tolerance = tolerance, ppm = ppm),
           "right" = .joinRight(x, y, tolerance = tolerance, ppm = ppm),
           "inner" = .joinInner(x, y, tolerance = tolerance, ppm = ppm)
    )
}

.joinLeft <- function(x, y, tolerance, ppm) {
    list(x = seq_along(x),
         y = closest(x, y, tolerance = tolerance, ppm = ppm,
                     duplicates = "closest"))
}

.joinRight <- function(x, y, tolerance, ppm) {
    list(x = closest(y, x, tolerance = tolerance, ppm = ppm,
                     duplicates = "closest"),
         y = seq_along(y))
}

.joinInner <- function(x, y, tolerance, ppm) {
    yi <- closest(y, x, tolerance = tolerance, ppm = ppm,
                  duplicates = "closest")
    notNa <- which(!is.na(yi))
    list(x = yi[notNa], y = notNa)
}

.joinOuter <- function(x, y, tolerance, ppm) {
    ji <- .joinInner(x, y, tolerance = tolerance, ppm = ppm)
    nx <- length(x)
    ny <- length(y)
    nlx <- length(ji[[1L]])
    xy <- xys <- c(x, y)
    ## equalise values that are identified as common
    if (nlx) {
        xy[nx + ji[[2L]]] <- xy[ji[[1L]]]
        xys <- xy[-(nx + ji[[2L]])]
    }
    ## find position
    i <- findInterval(xy, sort.int(xys))
    ## fill gaps with NA
    ox <- oy <- rep.int(NA_integer_, nx + ny - nlx)
    sx <- seq_len(nx)
    sy <- seq_len(ny)
    ox[i[sx]] <- sx
    oy[i[nx + sy]] <- sy
    list(x = ox, y = oy)
}
