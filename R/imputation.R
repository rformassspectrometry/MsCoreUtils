##' @title Quantitative mass spectrometry data imputation
##'
##' @description
##'
##' The `impute_matrix` function performs data imputation on `matrix`
##' objects instance using a variety of methods (see below).
##'
##' Users should proceed with care when imputing data and take
##' precautions to assure that the imputation produces valid results,
##' in particular with naive imputations such as replacing missing
##' values with 0.
##'
##' @section Types of missing values:
##'
##' There are two types of mechanisms resulting in missing values in
##' LC/MSMS experiments.
##'
##' - Missing values resulting from absence of detection of a feature,
##'   despite ions being present at detectable concentrations. For
##'   example in the case of ion suppression or as a result from the
##'   stochastic, data-dependent nature of the DDA MS acquisition
##'   method. These missing value are expected to be randomly
##'   distributed in the data and are defined, in statistical terms,
##'   as missing at random (MAR) or missing completely at random (MCAR).
##'
##' - Biologically relevant missing values resulting from the absence
##'   or the low abundance of ions (i.e. below the limit of detection of
##'   the instrument). These missing values are not expected to be
##'   randomly distributed in the data and are defined as missing not
##'   at random (MNAR).
##'
##' MNAR features should ideally be imputed with a left-censor method,
##' such as `QRILC` below. Conversely, it is recommended to use hot
##' deck methods such nearest neighbours, Bayesian missing value
##' imputation or maximum likelihood methods when values are missing
##' at random.
##'
##' @section Imputing by rows or columns:
##'
##' We assume that the input matrix `x` contains features along the
##' rows and samples along the columns, as is generally the case in
##' omics data analysis. When performing imputation, the missing
##' values are taken as a feature-specific property: feature *x* is
##' missing because it is absent (in a sample or group), or because it
##' was missed during acquisition (not selected during data dependent
##' acquisition) or data processing (not identified or with an
##' identification score below a chosen false discovery threshold). As
##' such, imputation is by default performed at the *feature
##' level*. In some cases, such as imputation by zero or a global
##' minimum value, it doesn't matter. In other cases, it does matter
##' very much, such as for example when using the minimum value
##' computed for each margin (i.e. row or column) as in the *MinDet*
##' method (see below) - do we want to use the minimum of the sample
##' or of that features? KNN is another such example: do we consider
##' the most similar features to impute a feature with missing values,
##' or the most similar samples to impute all missing in a sample.
##'
##' The `margin` argument can be used to change the imputation margin
##' from features/rows (`margin = 1`) to samples/columns (`margin = 2`).
##' Different imputations will have different default values, and
##' changing this parameter can have a major impact on imputation results
##' and downstream results.
##'
##' @section Imputation methods:
##'
##' Currently, the following imputation methods are available.
##'
##' - *MLE*: Maximum likelihood-based imputation method using the EM
##'   algorithm. Implemented in the `norm::imp.norm()`. function. See
##'   [norm::imp.norm()] for details and additional parameters. Note
##'   that here, `...` are passed to the [norm::em.norm()] function,
##'   rather to the actual imputation function `imp.norm`.
##'
##' - *bpca*: Bayesian missing value imputation are available, as
##'   implemented in the `pcaMethods::pca()` function. See
##'   [pcaMethods::pca()] for details and additional parameters.
##'
##' - *RF*: Random Forest imputation, as implemented in the
##'   `missForest::missForest` function. See [missForest::missForest()]] for
##'   details and additional parameters.
##'
##' - *knn*: Nearest neighbour averaging, as implemented in the
##'   `impute::impute.knn` function. See [impute::impute.knn()]] for
##'   details and additional parameters.
##'
##' - *QRILC*: A missing data imputation method that performs the
##'   imputation of left-censored missing data using random draws from
##'   a truncated distribution with parameters estimated using
##'   quantile regression. The `impute_QRILC()` function calls
##'   [imputeLCMD::impute.QRILC()] from the `imputeLCMD` package.
##'
##' - *MinDet*: Performs the imputation of left-censored missing data
##'   using a deterministic minimal value approach. Considering a
##'   expression data with *n* samples and *p* features, for each
##'   sample (for `margin = 2L`, the default), the missing entries are
##'   replaced with a minimal value observed in that sample. The
##'   minimal value observed is estimated as being the q-th quantile
##'   (default `q = 0.01`) of the observed values in that sample. The
##'   implemented in based on the `imputeLCMD::impute.MinDet()`
##'   function.
##'
##' - *MinProb*: Performs the imputation of left-censored missing data
##'   by random draws from a Gaussian distribution centred to a
##'   minimal value. Considering an expression data matrix with *n*
##'   samples and *p* features, for each sample, the mean value of the
##'   Gaussian distribution is set to a minimal observed value in that
##'   sample (for default `margin = 2L`, in the feature
##'   otherwise). The minimal value observed is estimated as being the
##'   q-th quantile (default `q = 0.01`) of the observed values in
##'   that sample (for `margin = 2L`, in the feature otherwise). The
##'   standard deviation is estimated as the median of the feature (or
##'   sample) standard deviations. Note that when estimating the
##'   standard deviation of the Gaussian distribution, only the
##'   peptides/proteins (or samples) which present more than 50\%
##'   recorded values are considered. The `impute_MinProb()` function
##'   calls [imputeLCMD::impute.MinProb()] from the `imputeLCMD`
##'   package.
##'
##' - *min*: Replaces the missing values with the smallest non-missing
##'   value in the data.
##'
##' - *zero*: Replaces the missing values with 0.
##'
##' - *mixed*: A mixed imputation applying two methods (to be defined
##'   by the user as `mar` for values missing at random and `mnar` for
##'   values missing not at random, see example) on two M[C]AR/MNAR
##'   subsets of the data (as defined by the user by a `randna`
##'   logical, of length equal to nrow(object)).
##'
##' - *nbavg*: Average neighbour imputation for fractions collected
##'   along a fractionation/separation gradient, such as sub-cellular
##'   fractions. The method assumes that the fraction are ordered
##'   along the gradient and is invalid otherwise.
##'
##'   Continuous sets `NA` value at the beginning and the end of the
##'   quantitation vectors are set to the lowest observed value in the
##'   data or to a user defined value passed as argument `k`. Then,
##'   when a missing value is flanked by two non-missing neighbouring
##'   values, it is imputed by the mean of its direct neighbours.
##'
##' - *with*: Replaces all missing values with a user-provided value.
##'
##' - *none*: No imputation is performed and the missing values are
##'   left untouched. Implemented in case one wants to only impute
##'   value missing at random or not at random with the *mixed*
##'   method.
##'
##' The `imputeMethods()` function returns a vector with valid
##' imputation method names.
##'
##' @references
##'
##' Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown,
##' Trevor Hastie, Robert Tibshirani, David Botstein and Russ B.
##' Altman, Missing value estimation methods for DNA microarrays
##' Bioinformatics (2001) 17 (6): 520-525.
##'
##' Oba et al., A Bayesian missing value estimation method for gene
##' expression profile data, Bioinformatics (2003) 19 (16): 2088-2096.
##'
##' Cosmin Lazar (2015). imputeLCMD: A collection of methods for
##' left-censored missing data imputation. R package version
##' 2.0. \url{http://CRAN.R-project.org/package=imputeLCMD}.
##'
##' Lazar C, Gatto L, Ferro M, Bruley C, Burger T. Accounting for the
##' Multiple Natures of Missing Values in Label-Free Quantitative
##' Proteomics Data Sets to Compare Imputation Strategies. J Proteome
##' Res. 2016 Apr 1;15(4):1116-25. doi:
##' 10.1021/acs.jproteome.5b00981. PubMed PMID:26906401.
##'
##' @rdname imputation
##'
##' @aliases imputeMethods impute_neighbour_average impute_knn impute_mle impute_bpca impute_mixed impute_min impute_zero impute_with impute_matrix impute_MinDet impute_MinProb impute_QRILC
##'
##' @useDynLib MsCoreUtils, .registration = TRUE
##'
##' @author Laurent Gatto
##'
##' @examples
##'
##' ## test data
##' set.seed(42)
##' m <- matrix(rlnorm(60), 10)
##' dimnames(m) <- list(letters[1:10], LETTERS[1:6])
##' m[sample(60, 10)] <- NA
##'
##' ## available methods
##' imputeMethods()
##'
##' impute_matrix(m, method = "zero")
##'
##' impute_matrix(m, method = "min")
##'
##' impute_matrix(m, method = "knn")
##'
##' ## same as impute_zero
##' impute_matrix(m, method = "with", val = 0)
##'
##' ## impute with half of the smalles value
##' impute_matrix(m, method = "with",
##'               val = min(m, na.rm = TRUE) * 0.5)
##'
##' ## all but third and fourth features' missing values
##' ## are the result of random missing values
##' randna <- rep(TRUE, 10)
##' randna[c(3, 9)] <- FALSE
##'
##' impute_matrix(m, method = "mixed",
##'               randna = randna,
##'               mar = "knn",
##'               mnar = "min")
##'
##' @param x A matrix or an `HDF5Matrix` object to be imputed.
##'
##' @param method `character(1)` defining the imputation method. See
##'     `imputeMethods()` for available ones.
##'
##' @param ... Additional parameters passed to the inner imputation
##'     function.
##'
##' @param margin `integer(1)` defining the margin along which to
##'     apply imputation, with `1L` for rows and `2L` for columns. The
##'     default value will depend on the imputation method. Use
##'     `getImputeMargin(fun)` to get the margin of imputation
##'     function `fun`. If the function doesn't take a margin
##'     argument, `NA` is returned.
##'
##' @return A matrix of same class as `x` with dimensions `dim(x)`.
##'
##' @export
impute_matrix <- function(x,
                          method,
                          FUN,
                          ...) {
    if (!anyNA(x)) return(x)
    ## Handle HDF5Matrix
    xIsHDF5 <- FALSE
    if (inherits(x, "HDF5Array")) {
        xIsHDF5 <- TRUE
        p <- HDF5Array::path(x) ## stored for later writing to disk
        ## Watch out this can lead to memory burst when x is large.
        x <- as.matrix(x)
    }
    ## User-provided imputation function
    if (!missing(FUN) && is.function(FUN)) {
        res <- impute_fun(x, FUN, ...)
        ## Write to HDF5 file if the input is on HDF5 backend
        if (xIsHDF5)
            res <- HDF5Array::writeHDF5Array(res, filepath = p,
                                             with.dimnames = TRUE)
        return(res)
    }
    ## Function name provided as a character
    if (missing(method))
        stop("Please specify an imputation method. ",
             "See '?impute_matrix' for details.")
    method <- match.arg(method,
                        choices = imputeMethods(),
                        several.ok = FALSE)
    res <- x
    if (method %in% c("CRILC", "MinProb"))
        requireNamespace("imputeLCMD")
    if (method == "knn") {
        res <- impute_knn(x, ...)
    } else if (method == "nbavg") {
        res <- impute_neighbour_average(x, ...)
    } else if (method == "MLE") {
        res <- impute_mle(x, ...)
    } else if (method == "bpca"){
        res <- impute_bpca(x, ...)
    } else if (method == "MinDet") {
        res <- impute_MinDet(x, ...)
    } else if (method == "MinProb") {
        res <- impute_MinProb(x, ...)
    } else if (method == "QRILC") {
        res <- imputeLCMD::impute.QRILC(x, ...)[[1]]
    } else if (method == "min") {
        res <- impute_min(x)
    } else if (method == "mixed") {
        res <- impute_mixed(x, ...)
    } else if (method == "zero") {
        res <- impute_zero(x)
    } else if (method == "with") {
        res <- impute_with(x, ...)
    } else if (method == "RF") {
        res <- impute_RF(x, ...)
    }
    ## else method == "none" -- do nothing
    ## Write to HDF5 file if the input is on HDF5 backend
    if (xIsHDF5)
        res <- HDF5Array::writeHDF5Array(res, filepath = p,
                                         with.dimnames = TRUE)
    res
}


##' @export
##' @rdname imputation
imputeMethods <- function()
    c("bpca","knn", "QRILC", "MLE",
      "MinDet", "MinProb", "min", "zero",
      "mixed", "nbavg", "with", "RF", "none")

##' @export
##' @rdname imputation
##' @param k `numeric(1)` providing the imputation value used for the
##'     first and last samples if they contain an `NA`. The default is
##'     to use the smallest value in the data.
impute_neighbour_average <- function(x, k = min(x, na.rm = TRUE), margin = 1L) {
    message("Assuming values are ordered.")
    margin <- .checkMargin(margin)
    if (margin == 2L) x <- t(x)
    res <- .Call("C_impNeighbourAvg", x, k)
    if (margin == 2L) return(t(res))
    else return(res)
}

##' @export
##' @rdname imputation
impute_knn <- function(x, margin = 1L, ...) {
    requireNamespace("impute")
    margin <- .checkMargin(margin)
    if (margin == 2L)
        x <- t(x)
    imp_res <- impute::impute.knn(x, ...)
    if (!is.null(imp_res$rng.state)) {
        assign(".Random.seed", imp_res$rng.state, envir = .GlobalEnv)
    } else {
        rm(".Random.seed", envir = .GlobalEnv)
    }
    if (margin == 2L)
        return(t(imp_res$data))
    else imp_res$data
}

##' @export
##' @rdname imputation
impute_mle <- function(x, margin = 2L, ...) {
    requireNamespace("norm")
    margin <- .checkMargin(margin)
    if (margin == 2L)
        x <- t(x)
    s <- norm::prelim.norm(x)  ## preliminary manipulations
    th <- norm::em.norm(s, ...) ## find the MLE
    seed <- sample(.Machine$integer.max, 1)
    norm::rngseed(seed) ## set random number generator seed
    res <- norm::imp.norm(s, th, x)  ## impute missing data under the MLE
    if (margin == 2L)
        res <- t(res)
    res
}

##' @export
##' @rdname imputation
impute_bpca <- function(x, margin = 1L, ...) {
    requireNamespace("pcaMethods")
    margin <- .checkMargin(margin)
    if (margin == 2L)
        x <- t(x)
    nSamples <- dim(x)[2]
    .resultBPCA <- pcaMethods::pca(x,
                                   method = "bpca",
                                   nPcs = (nSamples - 1),
                                   verbose = FALSE,
                                   ...)
    res <- pcaMethods::completeObs(.resultBPCA)
    if (margin == 2L)
        res <- t(res)
    res
}

##' @export
##' @rdname imputation
impute_RF <- function(x, margin = 2L, ...) {
    requireNamespace("missForest")
    margin <- .checkMargin(margin)
    if (margin == 2L)
        x <- t(x)
    res <- missForest::missForest(x, ...)$ximp
    if (margin == 2L)
        res <- t(res)
    res
}

##' @param randna `logical` of length equal to `nrow(object)` defining
##'     which rows are missing at random. The other ones are
##'     considered missing not at random. Only relevant when `methods`
##'     is `mixed`.
##'
##' @param mar Imputation method for values missing at random. See
##'     `method` above.
##'
##' @param mnar Imputation method for values missing not at
##'     random. See `method` above.
##'
##' @export
##'
##' @rdname imputation
impute_mixed <- function(x, randna, mar, mnar, margin = 1L, ...) {
    if (missing(randna))
        stop("Mixed imputation requires 'randna' argument. See ?impute_mixed.",
             call. = FALSE)
    stopifnot(is.logical(randna))
    if (missing(mar))
        stop("Mixed imputation requires 'mar' argument. See ?impute_mixed.",
             call. = FALSE)
    if (missing(mnar))
        stop("Mixed imputation requires 'mnar' argument. See ?impute_mixed.",
             call. = FALSE)
    suppressMessages(margin <- .checkMargin(margin))
    if (margin == 2L) x <- t(x)
    if (length(randna) != nrow(x))
        stop("Number of proteins and length of randna must be equal.",
             call. = FALSE)
    x[randna, ] <- impute_matrix(x[randna, ], mar,
                                 margin = margin, ...)
    x[!randna, ] <- impute_matrix(x[!randna, ], mnar,
                                  margin = margin, ...)
    if (margin == 2L) x <- t(x)
    x
}

##' @export
##'
##' @rdname imputation
impute_min <- function(x) {
    val <- min(x, na.rm = TRUE)
    x[is.na(x)] <- val
    x
}

##' @export
##'
##' @rdname imputation
##'
##' @importFrom stats quantile
##'
##' @param q `numeric(1)` indicating the quantile to be used to
##'     estimate the minimum in `MinDet` and `MinProb`. Default is
##'     0.01.
impute_MinDet <- function(x, q = 0.01, margin = 2L) {
    margin <- .checkMargin(margin)
    n <- dim(x)[margin]
    impVals <- apply(x, margin, quantile, prob = q, na.rm = TRUE)
    for (i in seq_len(n))
        x[is.na(x[, i]), i] = impVals[i]
    x
}

##' @export
##'
##' @rdname imputation
##'
##' @param sigma `numeric(1)` controling the standard deviation of the
##'     MNAR distribution in `MinProb` and `QRILC`. Default is 1.
impute_MinProb <- function (x, q = 0.01, sigma = 1, margin = 2L) {
    margin <- .checkMargin(margin)
    ## transpose if we want to non-default version
    if (margin == 1L) x <- t(x)
    x <- imputeLCMD::impute.MinProb(x, q = q, tune.sigma = sigma)
    if (margin == 1L) x <- t(x)
    x
}

##' @export
##'
##' @rdname imputation
impute_QRILC <- function(x, sigma = 1, margin = 2L) {
   margin <- .checkMargin(margin)
    ## transpose if we want to non-default version
    if (margin == 1L) x <- t(x)
    x <- imputeLCMD::impute.QRILC(x, tune.sigma = sigma)[[1]]
    if (margin == 1L) x <- t(x)
    x
}

##' @export
##'
##' @rdname imputation
impute_zero <- function(x) {
    x[is.na(x)] <- 0
    x
}

##' @export
##'
##' @rdname imputation
##'
##' @param val `numeric(1)` used to replace all missing values.
impute_with <- function(x, val) {
    if (missing(val))
        stop("Please provide a value.")
    x[is.na(x)] <- val
    x
}

##' @export
##'
##' @rdname imputation
##'
##' @param FUN A user-provided function that takes a `matrix` as input and
##'     returns an imputed `matrix` of identical dimensions.
##'
##' @examples
##'
##' ## user provided (random) imputation function
##' random_imp <- function(x) {
##'    m <- mean(x, na.rm = TRUE)
##'    sdev <- sd(x, na.rm = TRUE)
##'    n <- sum(is.na(x))
##'    x[is.na(x)] <- rnorm(n, mean = m, sd = sdev)
##'    x
##' }
##'
##' impute_matrix(m, FUN = random_imp)
impute_fun <- function(x, FUN, margin = 1L, ...) {
    margin <- .checkMargin(margin)
    if (margin == 2L) x <- t(x)
    res <- do.call(FUN, list(x, ...))
    if (margin == 2L) res <- t(res)
    stopifnot(identical(dim(x), dim(res)))
    res
}

.checkMargin <- function(margin) {
    margin <- as.integer(margin)
    if (!margin %in% c(1L, 2L))
        stop("'margin' must be 1L or 2L")
    message("Imputing along margin ", margin, ".")
    margin
}

##' @export
##'
##' @rdname imputation
##'
##' @param fun The imputation function to get the default margin from.
##'
##' @examples
##'
##' ## get the default margin
##' getImputeMargin(impute_knn) ## default imputes along features
##'
##' getImputeMargin(impute_mle) ## default imputes along samples
##'
##' getImputeMargin(impute_zero) ## NA: no margin here
getImputeMargin <- function(fun) {
    args <- formals(fun)
    i <- grep("margin", names(args))
    if (length(i)) ans <- args[[i]]
    else ans <- NA
    ans
}
