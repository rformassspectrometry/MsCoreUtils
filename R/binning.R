#' @title Binning
#'
#' @description
#'
#' Aggregate values in `x` for bins defined on `y`: all values
#' in `x` for values in `y` falling into a bin (defined on `y`) are
#' aggregated with the provided function `FUN`.
#'
#' @param x `numeric` with the values that should be aggregated/binned.
#'
#' @param y `numeric` with same length than `x` with values to be used for
#'     the binning.
#'
#' @param size `numeric(1)` with the size of a bin.
#'
#' @param breaks `numeric` defining the breaks (bins).
#'
#' @param FUN `function` to be used to aggregate values of `x` falling into the
#'     bins defined by `breaks`. `FUN` is expected to return a `numeric(1)`.
#'
#' @param returnMids `logical(1)` whether the midpoints for the breaks should be
#'     returned in addition to the binned (aggregated) values of `x`. Setting
#'     `returnMids = FALSE` might be useful if the breaks are defined before
#'     hand and binning needs to be performed on a large set of values (i.e.
#'     within a loop for multiple pairs of `x` and `y` values).
#'
#' @return
#'
#' Depending on the value of `returnMids`:
#'
#' - `returnMids = TRUE` (the default): returns a `list` with elements `x`
#'   (aggregated values of `x`) and `mids` (the bin mid points).
#' - `returnMids = FALSE`: returns a `numeric` with just the binned values for
#'   `x`.
#'
#' @author Johannes Rainer, Sebastian Gibb
#'
#' @export
#'
#' @rdname binning
#' @family grouping/matching functions
#' @examples
#'
#' ## Define example intensities and m/z values
#' ints <- abs(rnorm(20, mean = 40))
#' mz <- seq(1:length(ints)) + rnorm(length(ints), sd = 0.001)
#'
#' ## Bin intensities by m/z bins with a bin size of 2
#' bin(ints, mz, size = 2)
#'
#' ## Repeat but summing up intensities instead of taking the max
#' bin(ints, mz, size = 2, FUN = sum)
#'
#' ## Get only the binned values without the bin mid points.
#' bin(ints, mz, size = 2, returnMids = FALSE)
bin <- function(x, y, size = 1,
                breaks = seq(floor(min(y)),
                             ceiling(max(y)), by = size), FUN = max,
                returnMids = TRUE) {
    if (length(x) != length(y))
        stop("lengths of 'x' and 'y' have to match.")
    FUN <- match.fun(FUN)
    breaks <- .fix_breaks(breaks, range(y))
    nbrks <- length(breaks)
    idx <- findInterval(y, breaks)
    ## Ensure that indices are within breaks.
    idx[idx < 1L] <- 1L
    idx[idx >= nbrks] <- nbrks - 1L

    ints <- double(nbrks - 1L)
    ints[unique.default(idx)] <- vapply1d(split.default(x, idx), FUN)
    if (returnMids)
        list(x = ints, mids = (breaks[-nbrks] + breaks[-1L]) / 2L)
    else ints
}

#' Simple function to ensure that breaks (for binning) are spaning at least the
#' expected range.
#'
#' @param brks `numeric` with *breaks* such as calculated by `seq`.
#'
#' @param rng `numeric(2)` with the range of original numeric values on which
#'     the breaks were calculated.
#'
#' @noRd
.fix_breaks <- function(brks, rng) {
    ## Assuming breaks being sorted.
    if (brks[length(brks)] <= rng[2])
        brks <- c(brks, max((rng[2] + 1e-6),
                            brks[length(brks)] + mean(diff(brks))))
    brks
}
