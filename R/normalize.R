##' @export
##'
##' @rdname normalize
normalizeMethods <- function()
    c("sum", "max",
      "center.mean", "center.median",
      "div.mean", "div.median",
      "diff.median",
      "quantiles", "quantiles.robust",
      "vsn")


##' @title Quantitative data normalisation
##'
##' @description
##'
##' Function to normalise a matrix of quantitative omics data. The
##' nature of the normalisation is controlled by the `method`
##' argument, described below.
##'
##' @details
##'
##' The `method` parameter can be one of `"sum"`, `"max"`, `"center.mean"`,
##' `"center.median"`, `"div.mean"`, `"div.median"`, `"diff.meda"`,
##' `"quantiles`", `"quantiles.robust`" or `"vsn"`. The `normalizeMethods()`
##' function returns a vector of available normalisation methods.
##'
##' - For `"sum"` and `"max"`, each feature's intensity is divided by the
##'   maximum or the sum of the feature respectively. These two methods are
##'   applied along the features (rows).
##'
##' - `"center.mean"` and `"center.median"` center the respective sample
##'   (column) intensities by subtracting the respective column means or
##'   medians. `"div.mean"` and `"div.median"` divide by the column means or
##'   medians.
##'
##' - `"diff.median"` centers all samples (columns) so that they all match the
##'   grand median by subtracting the respective columns medians differences to
##'   the grand median.
##'
##' - Using `"quantiles"` or `"quantiles.robust"` applies (robust) quantile
##'   normalisation, as implemented in [preprocessCore::normalize.quantiles()]
##'   and [preprocessCore::normalize.quantiles.robust()]. `"vsn"` uses the
##'   [vsn::vsn2()] function.  Note that the latter also glog-transforms the
##'   intensities.  See respective manuals for more details and function
##'   arguments.
##'
##' @seealso The [scale()] function that centers (like `center.mean` above) and
##'     scales.
##'
##' @param x A matrix or an `HDF5Matrix` object to be normalised.
##'
##' @param method `character(1)` defining the normalisation
##'     method. See `normalizeMethods()` for available ones.
##'
##' @param ... Additional parameters passed to the inner normalisation
##'     function.
##'
##' @return A matrix of same class as `x` with dimensions `dim(x)`.
##'
##' @export
##'
##' @importFrom stats median
##'
##' @rdname normalize
##'
##' @author Laurent Gatto
##'
##' @examples
##' normalizeMethods()
##'
##' ## test data
##' set.seed(42)
##' m <- matrix(rlnorm(60), 10)
##'
##' normalize_matrix(m, method = "sum")
##'
##' normalize_matrix(m, method = "max")
##'
##' normalize_matrix(m, method = "quantiles")
##'
##' normalize_matrix(m, method = "center.mean")
normalize_matrix <- function(x, method, ...) {
    if (missing(method))
        stop("Please specify a normalization method. ",
             "See '?normalize_matrix' for details.")
    ## Handle HDF5Matrix
    xIsHDF5 <- FALSE
    if (inherits(x, "HDF5Array")) {
        xIsHDF5 <- TRUE
        p <- HDF5Array::path(x) ## stored for later writing to disk
        ## Watch out this can lead to memory burst when x is large
        x <- as.matrix(x)
    }

    method <- match.arg(method,
                        choices = normalizeMethods(),
                        several.ok = FALSE)

    if (method == "vsn") {
        stopifnot(requireNamespace("vsn"))
        e <- vsn::vsn2(x, ...)@hx
    } else if (method == "quantiles") {
        stopifnot(requireNamespace("preprocessCore"))
        e <- preprocessCore::normalize.quantiles(x, ...)
    } else if (method == "quantiles.robust") {
        stopifnot(requireNamespace("preprocessCore"))
        e <- preprocessCore::normalize.quantiles.robust(x, ...)
    } else if (method == "center.mean") {
        center <- colMeans(x, na.rm = TRUE)
        e <- sweep(x, 2L, center, FUN = "-", check.margin = FALSE, ...)
    } else if (method == "center.median") {
        center <- apply(x, 2L, median, na.rm = TRUE)
        e <- sweep(x, 2L, center, FUN = "-", check.margin = FALSE, ...)
    } else if (method == "div.mean") {
        center <- colMeans(x, na.rm = TRUE)
        e <- sweep(x, 2L, center, FUN = "/", check.margin = FALSE, ...)
    } else if (method == "div.median") {
        center <- apply(x, 2L, median, na.rm = TRUE)
        e <- sweep(x, 2L, center, FUN = "/", check.margin = FALSE, ...)
    } else if (method == "diff.median") {
        med <- median(as.numeric(x), na.rm = TRUE)
        cmeds <- apply(x, 2L, median, na.rm = TRUE)
        e <- sweep(x, 2L, cmeds - med, FUN = "-")
    } else { ## max or sum
        switch(method,
               max = div <- apply(x, 1, max, na.rm = TRUE),
               sum = div <- rowSums(x, na.rm = TRUE))
        e <- x/div
    }
    rownames(e) <- rownames(x)
    colnames(e) <- colnames(x)
    ## Write to HDF5 file if the input is on HDF5 backend
    if (xIsHDF5)
        e <- HDF5Array::writeHDF5Array(e, filepath = p,
                                       with.dimnames = TRUE)
    e
}
