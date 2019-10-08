#' @title Calculate the normalized dot product
#'
#' @description
#' Calculate the normalized dot product (NDP).`dotproduct` returns a numeric
#' value ranging between 0 and 1, where 0 indicates no similarity between the
#' two MS/MS features, while 1 indicates that the MS/MS features are identical.
#'
#' @param
#' x `matrix` with two column where one contains m/z values (column `"mz"`) and
#' the second corresponding intensity values (column `"intensity"`)
#'
#' @param
#' y `matrix` with two column where one contains m/z values (column `"mz"`) and
#' the second corresponding intensity values (column `"intensity"`)
#'
#' @param m `numeric(1)`, exponent for peak intensity-based weights
#'
#' @param n `numeric(1)`, exponent for m/z-based weights
#'
#' @details
#' Each row in `x` corresponds to the respective row in `y`, i.e. the peaks
#' (entries `"mz"`) per spectrum have to match.
#'
#' `m` and `n` are weights given on the peak intensity and the m/z values
#' respectively. As default (`m = 0.5`), the square root of the intensity
#' values are taken to calculate weights. With increasing values for `m`, high
#' intensity values become more important for the similarity calculation,
#' i.e. the differences between intensities will be aggravated.
#' With increasing values for `n`, high m/z values will be taken more into
#' account for similarity calculation. Especially when working with small
#' molecules, a value `n > 0` can be set, to give a weight on the m/z values to
#' accommodate that shared fragments with higher m/z are less likely and will
#' mean that molecules might be more similar. If `n != 0`, a warning will be
#' raised if the corresponding m/z values are not identical, since small
#' differences in m/z values will distort the similarity values with increasing
#' `n`. If `m=0` or `n=0`, intensity values or m/z values, respectively, are not
#' taken into account.
#'
#' The normalized dot product is calculated according to:
#' \deqn{NDP = \frac{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2}{ \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2) }}{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2 \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2)},
#' with \eqn{W = [ peak intensity] ^{m} \cdot [m/z]^n}.
#' For further information on normalized dot product see for example
#' Li et al. (2015).
#' Prior to calculating \deqn{W_{S1}} or \deqn{W_{S2}}, all intensity values
#' are divided by the maximum intensity value and multiplied by 100.
#'
#' @references
#' Li et al. (2015): Navigating natural variation in herbivory-induced
#' secondary metabolism in coyote tobacco populations using MS/MS structural
#' analysis. PNAS, E4147--E4155, \doi{10.1073/pnas.1503106112}.
#'
#' @return
#' `numeric(1)`, `dotproduct` returns a numeric similarity coefficient between
#' 0 and 1.
#'
#' @author Thomas Naake, \email{thomasnaake@@googlemail.com}
#' @family similarity functions
#' @export
#' @examples
#' x <- matrix(c(c(100.002, 100.001, NA, 300.01, 300.02, NA),
#'         c(2, 1.5, 0, 1.2, 0.9, 0)), ncol = 2,)
#' y <- matrix(c(c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
#'         c(2, 0, 3, 1, 4, 0.4)), ncol = 2)
#' colnames(x) <- colnames(y) <- c("mz", "intensity")
#' dotproduct(x, y, m = 0.5, n = 0)
dotproduct <- function(x, y, m = 0.5, n = 0) {

    ## check valid input
    if (!is.matrix(x)) stop("'x' is not a matrix")
    if (!is.matrix(y)) stop("'y' is not a matrix")

    if (nrow(x) != nrow(y)) stop("nrow(x) and nrow(y) are not identical")
    if (!is.numeric(m) || length(m) != 1)
        stop("`m` has to be a numeric of length 1.")
    if (!is.numeric(n) || length(n) != 1)
        stop("`n` has to be a numeric of length 1.")

    ## retrieve m/z and intensity from x and y
    mz1 <- x[, "mz"]
    mz2 <- y[, "mz"]
    inten1 <- x[, "intensity"]
    inten2 <- y[, "intensity"]

    ## check mz values: if mz1 and mz2 are not identical and the values are
    ## weighted by n, this might to unexpected results in the similarity
    ## calculation
    if (n && any(mz1 != mz2, na.rm = TRUE))
        warning("m/z values in `x` and `y` are not identical. ",
            "For n != 0 this might yield unexpected results.")

    inten1 <- inten1 / max(inten1, na.rm = TRUE) * 100
    inten2 <- inten2 / max(inten2, na.rm = TRUE) * 100

    ws1 <- inten1 ^ m * mz1 ^ n
    ws2 <- inten2 ^ m * mz2 ^ n

    ## calculate normalized dot product
    dp <- sum(ws1 * ws2, na.rm = TRUE)
    dp ^ 2 / (sum(ws1 ^ 2, na.rm = TRUE) * sum(ws2 ^ 2, na.rm = TRUE))
}
