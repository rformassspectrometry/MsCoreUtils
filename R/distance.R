#' @name distance
#' @title Spectra Distance/Similarity Measurements
#'
#' @description
#' These functions provide different similariy/distance measurements.
#'
#' @param x `matrix`, two-columns e.g. m/z, intensity
#' @param y `matrix`, two-columns e.g. m/z, intensity
#'
#' @details
#'
#' @return `double(1)` value between `0:1`, where `0` is completely different
#' and `1` identically.
#'
#' @rdname distance
#' @author Sebastian Gibb
#' @family distance/similarity functions
#' @references
#' Stein, S. E., and Scott, D. R. (1994).
#' Optimization and testing of mass spectral library search algorithms for
#' compound identification.
#' Journal of the American Society for Mass Spectrometry, 5(9), 859--866.
#' \doi{10.1016/1044-0305(94)87009-8}.
#'
#' Horai et al. (2010).
#' MassBank: a public repository for sharing mass spectral data for life
#' sciences. Journal of mass spectrometry, 45(7), 703--714.
#' \doi{10.1002/jms.1777}.
#'
#' Toprak et al. (2014).
#' Conserved peptide fragmentation as a benchmarking tool for mass spectrometers
#' and a discriminating feature for targeted proteomics.
#' Molecular & Cellular Proteomics : MCP, 13(8), 2056--2071.
#' \doi{10.1074/mcp.O113.036475}.
#'
#' @examples
#'
#' x <- matrix(c(1:5, 1:5), ncol = 2, dimnames = list(c(), c("mz", "intensity")))
#' y <- matrix(c(1:5, 5:1), ncol = 2, dimnames = list(c(), c("mz", "intensity")))
NULL

#' @rdname distance
#'
#' @param m `numeric`, weighting for the first column of `x` and `y` (e.g.
#' "mz"), default: `0` means don't weight by the first column.
#' @param n `numeric`, weighting for the second column of `x` and `y` (e.g.
#' "intensity), default: `0.5` means effectly using `sqrt(x[,2])` and
#' `sqrt(y[,2])`
#' @param na.rm `logical(1)`, should `NA` removed prior to calculation (default
#' `TRUE`).
#'
#' @details
#' `ndotproduct`: the normalized dot product is described in Stein and Scott
#' 1994 as: \eqn{NDP = \frac{\sum(W_1 W_2)^2}{\sum(W_1)^2 \sum(W_2)^2}}; where
#' \eqn{W_i = x^m * y^n}, where \eqn{x} and \eqn{y} are the m/z and intensity
#' values, respectively. Stein and Scott 1994 empirically determined the optimal
#' exponents as `m = 3` and `n = 0.6` by analyzing ca. 12000 EI-MS data of
#' 8000 organic compounds in the NIST Mass Spectral Library.
#' MassBank (Horai et al. 2010) uses `m = 2` and `n = 0.5`
#' for small compounds. In general with increasing values for `m`,
#' high m/z values will be taken more into account for similarity calculation.
#' Especially when working with small molecules, a value `n > 0` can be set
#' to give a weight on the m/z values to accommodate that shared fragments
#' with higher m/z are less likely and will mean that molecules might be more
#' similar. Increasing `n` will result in a higher importance of the intensity
#' values. Most commonly `m = 0` and `n = 0.5` are used.
#'
#' @author
#' `ndotproduct`: Sebastian Gibb and
#' Thomas Naake, \email{thomasnaake@@googlemail.com}
#'
#' @export
#' @aliases ndotproduct
#' @examples
#'
#' ndotproduct(x, y)
#' ndotproduct(x, y, m = 2, n = 0.5)
#' ndotproduct(x, y, m = 3, n = 0.6)
ndotproduct <- function(x, y, m = 0L, n = 0.5, na.rm = TRUE) {
    wx <- .weightxy(x[, 1L], x[, 2L], m, n)
    wy <- .weightxy(y[, 1L], y[, 2L], m, n)
    sum(wx * wy, na.rm = na.rm)^2L /
        (sum(wx^2L, na.rm = na.rm) * sum(wy^2L, na.rm = na.rm))
}

#' @rdname distance
#'
#' @details
#' `neuclidean`: the normalized euclidean distance is described in Stein and
#' Scott 1994 as:
#' \eqn{NED = (1 + \frac{\sum((W_1 - W_2)^2)}{sum((W_2)^2)})^{-1}}; where
#' \eqn{W_i = x^m * y^n}, where \eqn{x} and \eqn{y} are the m/z and intensity
#' values, respectively. See the details section about `ndotproduct` for an
#' explanation how to set `m` and `n`.
#'
#' @author
#' `neuclidean`: Sebastian Gibb
#'
#' @export
#' @aliases neuclidean
#' @examples
#'
#' neuclidean(x, y)
neuclidean <- function(x, y, m = 0L, n = 0.5, na.rm = TRUE) {
    wx <- .weightxy(x[, 1L], x[, 2L], m, n)
    wy <- .weightxy(y[, 1L], y[, 2L], m, n)
    1 / (1 + sum((wy - wx)^2L, na.rm = na.rm) / sum(wy^2L, na.rm = na.rm))
}

#' @rdname distance
#'
#' @details
#' `nspectraangle`: the normalized spectra angle is described in Toprak et al
#' 2014 as:
#' \eqn{NSA = 1 - \frac{2*\cos^{-1}(W_1 \cdot W_2)}{\pi}}; where
#' \eqn{W_i = x^m * y^n}, where \eqn{x} and \eqn{y} are the m/z and intensity
#' values, respectively. The weighting was not originally proposed by Toprak et
#' al. 2014. See the details section about `ndotproduct` for an explanation how
#' to set `m` and `n`.
#'
#' @author
#' `nspectraangle`: Sebastian Gibb
#'
#' @export
#' @aliases nspectraangle
#' @examples
#'
#' nspectraangle(x, y)
nspectraangle <- function(x, y, m = 0L, n = 0.5, na.rm = TRUE) {
    1 - 2 * acos(ndotproduct(x, y, m, n, na.rm = na.rm)) / pi
}

#' Calibrate function (workhorse of normalise)
#'
#' @param x `numeric`.
#' @param offset `numeric`.
#' @param scaling `numeric`.
#'
#' @return `numeric`.
#' @noRd
#' @examples
#' .calibrate(1:3, offset = 2, scaling = 2)
.calibrate <- function(x, offset = 0L, scaling = 1L) {
    (x - offset) / scaling
}

#' Weighted X Y function
#'
#' @param x `numeric`, e.g. mz values.
#' @param y `numeric`, e.g. intensity values.
#' @param m `numeric`, weighting `x`.
#' @param n `numeric`, weighting `y`.
#'
#' @return `numeric`.
#'
#' @references
#' Stein, S. E., and Scott, D. R. (1994).
#' Optimization and testing of mass spectral library search algorithms for
#' compound identification.
#' Journal of the American Society for Mass Spectrometry, 5(9), 859-866.
#' \doi{10.1016/1044-0305(94)87009-8}.
#'
#' @noRd
#' @examples
#' .weightxy(1:3, 4:6)
.weightxy <- function(x, y, m = 0, n = 0.5) {
    x ^ m * y ^ n
}
