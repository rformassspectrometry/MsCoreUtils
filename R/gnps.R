#' @title GNPS spectrum similarity scores
#'
#' @description
#'
#' The `join_gnps()`, `join_gnps_r()`, `gnps()` and `gnps_r()` functions allow
#' to calculate spectra similarity scores as used in
#' [GNPS](https://gnps.ucsd.edu/). The `_r` versions are the reference
#' implementation in R with full support of all parameters, while `join_gnps()`
#' and `gnps()` are implemented in C and therefore faster. The geneal approach
#' of the similarity calculation matches first peaks between the two spectra
#' directly using a user-defined `ppm` and/or `tolerance` as well as using a
#' fixed delta m/z (considering the same `ppm` and `tolerance`) that is
#' defined by the difference of the compared spectras' precursor m/z values.
#' For peaks that match multiple peaks in the other spectrum only the matching
#' peak pair with the higher value/similarity is considered in the final
#' similarity score calculation. Note that GNPS similarity scores are
#' calculated only if **both** functions are used together.
#'
#' - `join_gnps_r()`, `join_gnps()`: matches/maps peaks between spectra with
#'   the same approach as in GNPS: peaks are considered matching if a) the
#'   difference in their m/z values is smaller than defined by `tolerance`
#'   and `ppm` (this is the same as `joinPeaks()`) **and** b) the difference of
#'   their m/z *adjusted* for the difference of the spectras' precursor is
#'   smaller than defined by `tolerance` and `ppm`. Based on this definition,
#'   peaks in `x` can match up to two peaks in `y` hence returned peak indices
#'   might be duplicated. Note that if one of `xPrecursorMz` or `yPrecursorMz`
#'   are `NA` or if both are the same, the results are the same as with
#'   [join()]. The function returns a `list` of two `integer` vectors with the
#'   indices of the peaks matching peaks in the other spectrum or `NA`
#'   otherwise. The `join_gnps()` function is implemented in C and uses an
#'   *outer* join of the peaks (i.e., `type = "outer"`).
#'
#' - `gnps_r()`, gnps(): calculates the GNPS similarity score on peak matrices'
#'   previously *aligned* (matched) with `join_gnps()`. For multi-mapping peaks
#'   the pair with the higher similarity are considered in the final score
#'   calculation. By setting `matchedPeaksCount = TRUE` the number of peak pairs
#'   on which the score was calculated is returned in addition to the
#'   similarity score. By default (with `matchedPeaksCount = FALSE`) a
#'   `numeric(1)` with the similarity score is returned. For
#'   `matchedPeaksCount = TRUE` a `numeric(2)` is returned with the first
#'   element being the similarity scoreand the second the number of matched peak
#'   pairs. The `gnps()` function is implemented in C while the `gnps_r()`
#'   function is based on the implementation from the references below.
#'
#' @details
#'
#' The implementation of `gnps_r()` bases on the R code from the publication
#' listed in the references.
#'
#' @param ppm for `join_gnps()` and `join_gnps_r()`: `numeric(1)` defining a
#'     relative, m/z-dependent, maximal accepted difference between m/z values
#'     of peaks from the two spectra to be matched/mapped.
#'
#' @param tolerance for `join_gnps()` and `join_gnps_r()`: `numeric(1)`
#'     defining a constant maximal accepted difference between m/z values of
#'     peaks from the two spectra to be matched/mapped.
#'
#' @param type for `join_gnps_r()`: `character(1)` specifying the type of join
#'     that should be performed. See [join()] for details and options. Defaults
#'     to `type = "outer"`. `join_gnps()` only uses `type = "outer"`.
#'
#' @param x for `join_gnps()` and `join_gnps_r()`: `numeric` with m/z values
#'     from a spectrum. For `gnps()` and `gnps_r()`: `matrix` with two
#'     columns `"mz"` and `"intensity"` containing the peaks **aligned** with
#'     peaks in `y` (with `join_gnps()` or `join_gnps_r()`).
#'
#' @param xPrecursorMz for `join_gnps()` and `join_gnps_r()`: `numeric(1)` with
#'     the precursor m/z of the spectrum `x`.
#'
#' @param y for `join_gnps()` and `join_gnps_r()`: `numeric` with m/z values
#'     from a spectrum. For `gnps()` and `gnps_r()`: `matrix` with two columns
#'     `"mz"` and `"intensity"` containing the peaks **aligned** with peaks
#'     in `x` (with `join_gnps()` or `join_gnps_r()`).
#'
#' @param yPrecursorMz for `join_gnps()` or `join_gnps_r()`: `numeric(1)` with
#'     the precursor m/z of the spectrum `y`.
#'
#' @param matchedPeaksCount `logical(1)` whether the number of peak pairs on
#'     which the score was calculated should be returned. Defaults to
#'     `matchedPeaksCount = FALSE`. If set to `matchedPeaksCount = TRUE` a
#'     `numeric` of length 2 is returned.
#'
#' @param ... for `join_gnps()` and `join_gnps_r()`: optional parameters passed
#'     to the [join()] function. For `gnps()` and `gnps_r()`: ignored.
#'
#' @author Johannes Rainer, Michael Witting, based on the code from
#'     Xing *et al.* (2020). Adriano Rutz for the C implementations.
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
#' @seealso [gnps_chain_dp()] for am optimized and fast implementation based
#'     on the Chain-DP algorithm.
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
#' ## join_gnps_r finds 4 matches
#' join_gnps_r(x[, 1], y[, 1], pmz_x, pmz_y)
#'
#' ## with one of the two precursor m/z being NA, the result are the same as
#' ## with join.
#' join_gnps_r(x[, 1], y[, 1], pmz_x, yPrecursorMz = NA)
#'
#' ## Calculate GNPS similarity score:
#' map <- join_gnps_r(x[, 1], y[, 1], pmz_x, pmz_y)
#' gnps_r(x[map[[1]], ], y[map[[2]], ])
gnps_r <- function(x, y, ..., matchedPeaksCount = FALSE) {
    if (nrow(x) != nrow(y)) {
        stop("'x' and 'y' are expected to have the same number of rows).")
    }
    x_sum <- sum(x[!duplicated(x[, 1L]), 2L], na.rm = TRUE)
    y_sum <- sum(y[!duplicated(y[, 1L]), 2L], na.rm = TRUE)
    if (x_sum == 0 || y_sum == 0)
        return(0.0)
    ## Keep only matches.
    keep <- which(!is.na(x[, 1L]) & !is.na(y[, 1L]))
    if (!length(keep))
        return(0.0)
    x <- x[keep, , drop = FALSE]
    y <- y[keep, , drop = FALSE]
    scores <- sqrt(x[, 2L]) / sqrt(x_sum) * sqrt(y[, 2L]) / sqrt(y_sum)
    x_idx <- match(x[, 1L], unique(x[, 1L]))
    y_idx <- match(y[, 1L], unique(y[, 1L]))
    n <- length(keep)
    score_mat <- matrix(0.0, nrow = n, ncol = n)
    score_mat[(y_idx - 1L) * n + x_idx] <- scores
    best <- solve_LSAP(score_mat, maximum = TRUE)
    scrs <- score_mat[(best - 1L) * n + seq_len(n)]
    res <- sum(scrs, na.rm = TRUE)
    if (matchedPeaksCount)
        return(c(res, sum(scrs > 0, na.rm = TRUE)))
    res
}

#' @rdname gnps
#'
#' @export
join_gnps_r <- function(x, y, xPrecursorMz = NA_real_, yPrecursorMz = NA_real_,
                        tolerance = 0, ppm = 0, type = "outer", ...) {
    pdiff <- yPrecursorMz - xPrecursorMz
    map <- join(x, y, tolerance = tolerance, ppm = ppm, type = type, ...)
    if (is.finite(pdiff) && pdiff != 0) {
        pmap <- join(
            x + pdiff,
            y,
            tolerance = tolerance,
            ppm = ppm,
            type = type,
            ...
        )
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

#' @rdname gnps
#'
#' @export
join_gnps <- function(x, y, xPrecursorMz = NA_real_, yPrecursorMz = NA_real_,
                      tolerance = 0, ppm = 0, ...) {
    .Call(
        C_join_gnps,
        x = x,
        y = y,
        xPrecursorMz = xPrecursorMz,
        yPrecursorMz = yPrecursorMz,
        tolerance = tolerance,
        ppm = ppm
    )
}

#' @rdname gnps
#'
#' @export
gnps <- function(x, y, ..., matchedPeaksCount = FALSE) {
    res <- .Call(C_gnps, x = x, y = y)
    if (matchedPeaksCount) res else res[1L]
}

#' @title Optimized GNPS Modified Cosine Similarity via Chain-DP
#'
#' @description
#'
#' Computes the GNPS (Global Natural Products Social molecular networking)
#' modified cosine similarity score between two mass spectra using a fused
#' join + score algorithm based on Chain-DP (Chain Dynamic Programming).
#' This function combines peak matching and scoring in a single C call,
#' achieving consequent speedup over the standard [gnps()]
#' implementation while maintaining exact mathematical equivalence
#' (`differences <= 2.2e-16`).
#'
#' **Algorithm**: Chain-DP optimal assignment. When spectra are sanitized,
#' the bipartite matching graph forms simple chains (not arbitrary networks).
#' This enables O(n+m) greedy scoring for most of the pairs, with exact
#' Hungarian solver O(k³) only for rare conflicts (k about 3–5).
#'
#' **Complexity**: O(n+m) time, O(n+m) memory (vs. O(n³) time, O(n²) memory
#' for full Hungarian).
#'
#' @section Prerequisites:
#'
#' **CRITICAL**: Input spectra MUST be sanitized before calling this function:
#'
#' - **Unique m/z values**: no two peaks in the same spectrum should have
#'   m/z values close enough to match each other (i.e., `|mz_i - mz_j| >
#'     tolerance` for all peak pairs i,j within the same spectrum)
#'
#' - **Non-negative intensities** (no NaN/NA/Inf)
#'
#' - **Sorted by m/z** in ascending order
#'
#' The chain-DP algorithm assumes at most one direct match and one shifted
#' match per peak — a property that holds when peaks are well-separated.
#' Unsanitized spectra will produce incorrect scores silently.
#'
#' **How to sanitize**:
#' \preformatted{
#' library(Spectra)
#' sps <- reduceSpectra(sps)   # Remove peaks closer than tolerance
#' sps <- combinePeaks(sps)    # Merge duplicate m/z
#' sps <- scalePeaks(sps)      # Normalize intensities
#' }
#'
#' @param x Numeric `matrix` with query spectrum peaks (2 columns: mz,
#'     intensity). Must be sorted by mz in ascending order.
#'
#' @param y Numeric `matrix` with library spectrum peaks (2 columns: mz,
#'     intensity). Must be sorted by mz in ascending order.
#'
#' @param xPrecursorMz `numeric(1)`, precursor m/z for query spectrum.
#'
#' @param yPrecursorMz `numeric(1)`, precursor m/z for library spectrum.
#'
#' @param tolerance `numeric(1)`, absolute tolerance in Daltons.
#'
#' @param ppm `numeric(1)`, relative tolerance in ppm.
#'
#' @param matchedPeaksCount `logical(1)`; if `TRUE`, return both score and
#'   matched-peak count, otherwise return score only.
#'
#' @param ... ignored.
#'
#' @return A `numeric` vector of length 1 by default (score), or length 2 when
#'   `matchedPeaksCount = TRUE` (`c(score, matched_peaks)`).
#'
#' @details
#'
#' The modified cosine score is computed as:
#'
#' \deqn{
#'   \text{score}(i,j) = \frac{\sqrt{I_x(i)}}{\sqrt{\sum I_x}}
#'                     \times \frac{\sqrt{I_y(j)}}{\sqrt{\sum I_y}}
#' }
#'
#' where the sum is over unique m/z values (first occurrence of duplicates).
#'
#' The total score is the sum of all optimally assigned peak pairs, found via:
#'
#' \enumerate{
#'   \item **Direct matching**: `join(x, y, type="outer")` - closest one-to-one
#'   \item **Shifted matching**: `join(x + pdiff, y, type="outer")` where
#'         `pdiff = yPrecursorMz - xPrecursorMz`
#'   \item **Optimal assignment via Chain-DP**: For each query peak, pick the
#'         better of its direct and shifted matches. When spectra are sanitized,
#'         conflicts are rare (~1%) and resolved optimally with exact Hungarian.
#' }
#'
#' **Precursor threshold**: Shifted matching is skipped when
#' `|pdiff| <= tolerance + ppm × max(xPrecursorMz, yPrecursorMz) × 1e-6`,
#' i.e., when the precursor difference is within the peak matching tolerance.
#' This is scientifically correct (no meaningful neutral loss when
#' pdiff close to tolerance) but differs from the existing `gnps()`
#' implementation, which only skips when `pdiff == 0.0` exactly. See
#' References for details.
#'
#' @author Adriano Rutz
#'
#' @seealso
#'
#' [gnps()] for the standard (backward-compatible) implementation.
#'
#' [join_gnps()] for peak matching only.
#'
#' @references
#'
#' Wang M, Carver JJ, Phelan VV, et al. (2016). "Sharing and community curation
#' of mass spectrometry data with Global Natural Products Social Molecular
#' Networking." *Nature Biotechnology* 34:828–837. \doi{10.1038/nbt.3597}
#'
#' Dührkop K, Fleischauer M, Ludwig M, et al. (2019). "SIRIUS 4: a rapid tool
#' for turning tandem mass spectra into metabolite structure information."
#' *Nature Methods* 16:299–302. \doi{10.1038/s41592-019-0344-8}
#'
#' Chain-DP algorithm implementation:
#' \url{https://github.com/sirius-ms/sirius/blob/stable/spectral_alignment/src/main/java/de/unijena/bionf/fastcosine/FastCosine.java}
#'
#' @examples
#' # Example spectra (sanitized: sorted, unique m/z, no NAs)
#' x <- cbind(mz = c(10, 36, 63, 91, 93), intensity = c(14, 15, 999, 650, 1))
#' y <- cbind(mz = c(10, 12, 50, 63, 105), intensity = c(35, 5, 16, 999, 450))
#'
#' # Compute modified cosine via chain-DP (hot path)
#' result <- gnps_chain_dp(x, y,
#'                         xPrecursorMz = 91.0,
#'                         yPrecursorMz = 105.0,
#'                         tolerance = 0.01,
#'                         ppm = 10,
#'                         matchedPeaksCount=TRUE)
#' result[1L]
#' result[2L]
#'
#' # Compare with standard implementation (should agree within 1e-15)
#' matches <- join_gnps(x[,1], y[,1], 91.0, 105.0, 0.01, 10)
#' score_std <- gnps(x[matches$x, ], y[matches$y, ])
#' abs(result[1L] - score_std) < 1e-10  # TRUE
#'
#' @export
gnps_chain_dp <- function(x, y, xPrecursorMz = NA_real_,
                          yPrecursorMz = NA_real_, tolerance = 0.0, ppm = 0.0,
                          ..., matchedPeaksCount = FALSE) {
    res <- .Call(
        C_gnps_chain_dp,
        x = x,
        y = y,
        xPrecursorMz = xPrecursorMz,
        yPrecursorMz = yPrecursorMz,
        tolerance = tolerance,
        ppm = ppm
    )
    if (matchedPeaksCount) res else res[1L]
}
