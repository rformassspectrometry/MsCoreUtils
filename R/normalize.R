##' @export
##' 
##' @rdname normalize
normalizeMethods <- function()
    c("sum", "max", "center.mean",
      "center.median", "diff.median",
      "quantiles", "quantiles.robust", "vsn")


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
##' The `method` parameter in `normalize` can be one of `"sum"`,
##' `"max"`, `"quantiles"`, `"center.mean"`, `"center.median"`,
##' `"center.median"`, `"quantiles.robust`" or `"vsn"`.  For `"sum"`
##' and `"max"`, each feature's intensity is divided by the maximum or
##' the sum of the feature respectively. These two methods are applied
##' along the features (rows). The `normalizeMethods()` function
##' returns a vector of available normalisation methods.
##'
##' `"center.mean"` and `"center.median"` translate the respective
##' sample (column) intensities according to the column mean or
##' median. `"diff.median"` translates all samples (columns) so that
##' they all match the grand median. Using `"quantiles"` or
##' `"quantiles.robust"` applies (robust) quantile normalisation, as
##' implemented in [preprocessCore::normalize.quantiles()] and
##' [preprocessCore::normalize.quantiles.robust()]. `"vsn"` uses the
##' [vsn::vsn2()] function.  Note that the latter also glog-transforms
##' the intensities.  See respective manuals for more details and
##' function arguments.
##' 
##' @param x A matrix to be normalised.
##'
##' @param method `character(1)` defining the normalisation
##'     method. See `normalizeMethods()` for available ones.
##' 
##' @param ... Additional parameters passed to the inner normalisation
##'     function.
##' 
##' @return A normalised matrix of dimensions `dim(x)`.
##'
##' @export
##' 
##' @rdname normalize
##' 
##' @author Laurent Gatto
##'
##' @examples
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
    if (method == "vsn") {
        e <- Biobase::exprs(vsn::vsn2(x, ...))
    } else if (method == "quantiles") {
        e <- preprocessCore::normalize.quantiles(x, ...)
    } else if (method == "quantiles.robust") {
        e <- preprocessCore::normalize.quantiles.robust(x, ...)
    } else if (method == "center.mean") {
        center <- colMeans(x, na.rm = TRUE)
        e <- sweep(x, 2L, center, check.margin = FALSE, ...)
    } else if (method == "center.median") {
        center <- apply(x, 2L, median, na.rm = TRUE)
        e <- sweep(e, 2L, center, check.margin = FALSE, ...)
    } else if (method == "diff.median") {
        med <- median(as.numeric(x), na.rm = TRUE)
        cmeds <- apply(x, 2L, median, na.rm = TRUE)
        e <- sweep(x, 2L, cmeds - med)
    } else {
        switch(method,
               max = div <- apply(x, 1, max, na.rm = TRUE),
               sum = div <- rowSums(x, na.rm = TRUE))
        e <- x/div
    }
    rownames(e) <- rownames(x)
    colnames(e) <- colnames(x)
    e
}
