#' @title GNPS spectrum similarity scores
#'
#' @description
#'
#' The `join_gnps` and `gnps` functions allow to calculate spectra similarity
#' scores as used in [GNPS](https://gnps.ucsd.edu/). The approach matches first
#' peaks between the two spectra directly using a user-defined ppm and/or
#' tolerance as well as using a fixed delta m/z (considering the same ppm and
#' tolerance) that is defined by the difference of the two spectras' precursor
#' m/z values. For peaks that match multiple peaks in the
#' other spectrum only the matching peak pair with the higher value/similarity
#' is considered in the final similarity score calculation. Note that GNPS
#' similarity scores are calculated only if the two functions are used together.
#'
#' - `join_gnps`: matches/maps peaks between spectra with the same approach
#'   as in GNPS: peaks are considered matching if a) the
#'   difference in their m/z values is smaller than defined by `tolerance`
#'   and `ppm` (this is the same as `joinPeaks`) **and** b) the difference of
#'   their m/z *adjusted* for the difference of the spectras' precursor is
#'   smaller than defined by `tolerance` and `ppm`. Based on this definition,
#'   peaks in `x` can match up to two peaks in `y` hence returned peak indices
#'   might be duplicated. Note that if one of `xPrecursorMz` or `yPrecursorMz`
#'   are `NA` or if both are the same, the results are the same as with
#'   [join()]. The function returns a `list` of two `integer` vectors with the
#'   indices of the peaks matching peaks in the other spectrum or `NA`
#'   otherwise.
#'
#' - `gnps`: calculates the GNPS similarity score on peak matrices' previously
#'   *aligned* (matched) with `join_gnps`. For multi-mapping peaks the pair with
#'   the higher similarity are considered in the final score calculation.
#'
#' @details
#'
#' The implementation of `gnps` bases on the R code from the publication listed
#' in the references.
#'
#' @param ppm for `join_gnps`: `numeric(1)` defining a relative, m/z-dependent,
#'     maximal accepted difference between m/z values of peaks from the two
#'     spectra to be matched/mapped.
#'
#' @param tolerance for `join_gnps`: `numeric(1)` defining a constant maximal
#'     accepted difference between m/z values of peaks from the two spectra to
#'     be matched/mapped.
#'
#' @param type for `join_gnps`: `character(1)` specifying the type of join that
#'     should be performed. See [join()] for details and options. Defaults to
#'     `type = "outer"`.
#'
#' @param x for `join_gnps`: `numeric` with m/z values from a spectrum. For
#'     `gnps`: `matrix` with two columns `"mz"` and `"intensity"` containing
#'     the peaks **aligned** with peaks in `y` (with `join_gnps`).
#'
#' @param xPrecursorMz for `join_gnps`: `numeric(1)` with the precursor m/z
#'     of the spectrum `x`.
#'
#' @param y for `join_gnps`: `numeric` with m/z values from a spectrum. For
#'     `gnps`: `matrix` with two columns `"mz"` and `"intensity"` containing
#'     the peaks **aligned** with peaks in `x` (with `join_gnps`).
#'
#' @param yPrecursorMz for `join_gnps`: `numeric(1)` with the precursor m/z
#'     of the spectrum `y`.
#'
#' @param ... for `join_gnps`: optional parameters passed to the [join()]
#'     function. For `gnps`: ignored.
#'
#' @author Johannes Rainer, Michael Witting, based on the code from
#'     Xing *et al.* (2020).
#'
#' @importFrom clue solve_LSAP
#'
#' @importFrom stats complete.cases
#'
#' @references
#'
#' Xing S, Hu Y, Yin Z, Liu M, Tang X, Fang M, Huan T. Retrieving and Utilizing
#' Hypothetical Neutral Losses from Tandem Mass Spectra for Spectral Similarity
#' Analysis and Unknown Metabolite Annotation. *Anal Chem.*
#' 2020 Nov 3;92(21):14476-14483. \doi{10.1021/acs.analchem.0c02521}.
#'
#' @family grouping/matching functions
#'
#' @family distance/similarity functions
#'
#' @return
#'
#' See function definition in the description section.
#'
#' @rdname gnps
#'
#' @export
#'
#' @examples
#'
#' ## Define spectra
#' x <- cbind(mz = c(10, 36, 63, 91, 93), intensity = c(14, 15, 999, 650, 1))
#' y <- cbind(mz = c(10, 12, 50, 63, 105), intensity = c(35, 5, 16, 999, 450))
#' ## The precursor m/z
#' pmz_x <- 91
#' pmz_y <- 105
#'
#' ## Plain join identifies only 2 matching peaks
#' join(x[, 1], y[, 1])
#'
#' ## join_gnps finds 4 matches
#' join_gnps(x[, 1], y[, 1], pmz_x, pmz_y)
#'
#' ## with one of the two precursor m/z being NA, the result are the same as
#' ## with join.
#' join_gnps(x[, 1], y[, 1], pmz_x, yPrecursorMz = NA)
#'
#' ## Calculate GNPS similarity score:
#' map <- join_gnps(x[, 1], y[, 1], pmz_x, pmz_y)
#' gnps(x[map[[1]], ], y[map[[2]], ])
gnps <- function(x, y, ...) {
    if (nrow(x) != nrow(y))
        stop("'x' and 'y' are expected to have the same number of rows).")
    ## Scale intensities; !duplicated because we can have duplicated matches.
    x_sum <- sum(x[!duplicated(x[, 1]), 2], na.rm = TRUE)
    y_sum <- sum(y[!duplicated(y[, 1]), 2], na.rm = TRUE)
    ## is 0 if only NAs in input - avoids division through 0
    if (x_sum == 0 || y_sum == 0)
        return(0)
    ## Keep only matches.
    keep <- which(complete.cases(cbind(x[, 1], y[, 1])))
    l <- length(keep)
    if (!l)
        return(0)
    x <- x[keep, , drop = FALSE]
    y <- y[keep, , drop = FALSE]
    scores <- sqrt(x[, 2]) / sqrt(x_sum) * sqrt(y[, 2]) / sqrt(y_sum)

    x_idx <- as.integer(factor(x[, 1]))
    y_idx <- as.integer(factor(y[, 1]))
    score_mat <- matrix(0, nrow = l, ncol = l)
    seq_l <- seq_len(l)
    for (i in seq_l) {
        score_mat[x_idx[i], y_idx[i]] <- scores[i]
    }
    best <- solve_LSAP(score_mat, maximum = TRUE)
    sum(score_mat[cbind(seq_l, as.integer(best))], na.rm = TRUE)
}

#' @rdname gnps
#'
#' @export
join_gnps <- function(x, y, xPrecursorMz = NA_real_, yPrecursorMz = NA_real_,
                      tolerance = 0, ppm = 0, type = "outer", ...) {
    pdiff <- yPrecursorMz - xPrecursorMz
    map <- join(x, y, tolerance = tolerance, ppm = ppm,
                type = type, ...)
    if (is.finite(pdiff) && pdiff != 0) {
        pmap <- join(x + pdiff, y, tolerance = tolerance,
                     ppm = ppm, type = type, ...)
        ## Keep only matches here
        nona <- !(is.na(pmap[[1L]]) | is.na(pmap[[2L]]))
        if (any(nona)) {
            map[[1L]] <- c(map[[1L]], pmap[[1L]][nona])
            map[[2L]] <- c(map[[2L]], pmap[[2L]][nona])
            idx <- order(map[[1L]])
            map[[1L]] <- map[[1L]][idx]
            map[[2L]] <- map[[2L]][idx]
        }
    }
    map
}
