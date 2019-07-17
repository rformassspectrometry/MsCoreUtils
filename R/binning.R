#' @title Binning
#'
#' @description
#'
#' Aggregate values in `x` for bins defined on `toBin`: all values
#' in `x` for values in `toBin` falling into a bin (defined on `toBin`) are
#' aggregated with the provided function `FUN`.
#'
#' @param x `numeric` with the values that should be aggregated/binned.
#'
#' @param toBin `numeric` with same length than `x` with values to be used for
#'     the binning.
#'
#' @param binSize `numeric(1)` with the size of a bin.
#'
#' @param breaks `numeric` defining the breaks (bins).
#'
#' @param FUN `function` to be used to aggregate values of `x` falling into the
#'     bins defined by `breaks`.
#'
#' @return `list` with elements `x` (aggregated values of `x`) and `mids` (the
#'     bin mid points).
#'
#' @author Johannes Rainer, Sebastian Gibbals
#'
#' @export
#'
#' @rdname binning
#'
#' @export
#'
#' ## Define example intensities and m/z values
#' ints <- abs(rnorm(20, mean = 40))
#' mz <- seq(1:length(vals)) + rnorm(length(vals), sd = 0.001)
#'
#' ## Bin intensities by m/z bins with a binSize of 2
#' bin(ints, mz, binSize = 2)
#'
#' ## Repeat but summing up intensities instead of taking the max
#' bin(ints, mz, binSize = 2, FUN = sum)
bin <- function(x, toBin, binSize = 1,
                breaks = seq(floor(min(toBin)),
                             ceiling(max(toBin)), by = binSize), FUN = max) {
    if (length(x) != length(toBin))
        stop("lengths of 'x' and 'toBin' have to match.")
    FUN <- match.fun(FUN)
    breaks <- .fix_breaks(breaks, range(toBin))
    nbrks <- length(breaks)
    idx <- findInterval(toBin, breaks)
    ## Ensure that indices are within breaks.
    idx[idx < 1L] <- 1L
    idx[idx >= nbrks] <- nbrks - 1L

    ints <- double(nbrks - 1L)
    ints[unique(idx)] <- unlist(lapply(base::split(x, idx), FUN),
                                use.names = FALSE)
    list(x = ints, mids = (breaks[-nbrks] + breaks[-1L]) / 2L)
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
