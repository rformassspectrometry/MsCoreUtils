##' @title Quantitative mass spectrometry data imputation
##'
##' @description
##'
##' The `impute_matrix` function performs data imputation on `matrix`
##' objects instance using a variety of methods (see below).
##'
##' Users should proceed with care when imputing data and take
##' precautions to assure that the imputation produce valid results,
##' in particular with naive imputations such as replacing missing
##' values with 0.
##'
##' @details
##'
##' There are two types of mechanisms resulting in missing values in
##' LC/MSMS experiments.
##'
##' - Missing values resulting from absence of detection of a feature,
##'   despite ions being present at detectable concentrations. For
##'   example in the case of ion suppression or as a result from the
##'   stochastic, data-dependent nature of the MS acquisition
##'   method. These missing value are expected to be randomly
##'   distributed in the data and are defined as missing at random
##'   (MAR) or missing completely at random (MCAR).
##'
##' - Biologically relevant missing values resulting from the absence
##'   of the low abundance of ions (below the limit of detection of
##'   the instrument). These missing values are not expected to be
##'   randomly distributed in the data and are defined as missing not
##'   at random (MNAR).
##'
##' MNAR features should ideally be imputed with a left-censor method,
##' such as `QRILC` below. Conversely, it is recommended to use host
##' deck methods such nearest neighbours, Bayesian missing value
##' imputation or maximum likelihood methods when values are missing
##' at random.
##'
##' Currently, the following imputation methods are available.
##'
##' - *MLE*: Maximum likelihood-based imputation method using the EM
##'   algorithm. Implemented in the `norm::imp.norm()`. function. See
##'   [norm::imp.norm()] for details and additional parameters. Note
##'   that here, `...` are passed to the [norm::em.norm()` function,
##'   rather to the actual imputation function `imp.norm`.
##'
###' - *bpca*: Bayesian missing value imputation are available, as
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
##'   quantile regression. Implemented in the
##'   `imputeLCMD::impute.QRILC`
##'   function. [imputeLCMD::impute.QRILC()] for details and
##'   additional parameters.
##'
##' - *MinDet*: Performs the imputation of left-censored missing data
##'   using a deterministic minimal value approach. Considering a
##'   expression data with *n* samples and *p* features, for each
##'   sample, the missing entries are replaced with a minimal value
##'   observed in that sample. The minimal value observed is estimated
##'   as being the q-th quantile (default `q = 0.01`) of the observed
##'   values in that sample. Implemented in the
##'   `imputeLCMD::impute.MinDet` function. See
##'   [imputeLCMD::impute.MinDet()] for details and additional
##'   parameters.
##'
##' - *MinProb*: Performs the imputation of left-censored missing data
##'   by random draws from a Gaussian distribution centred to a
##'   minimal value. Considering an expression data matrix with *n*
##'   samples and *p* features, for each sample, the mean value of the
##'   Gaussian distribution is set to a minimal observed value in that
##'   sample. The minimal value observed is estimated as being the
##'   q-th quantile (default `q = 0.01`) of the observed values in
##'   that sample. The standard deviation is estimated as the median
##'   of the feature standard deviations. Note that when estimating
##'   the standard deviation of the Gaussian distribution, only the
##'   peptides/proteins which present more than 50\% recorded values
##'   are considered. Implemented in the `imputeLCMD::impute.MinProb`
##'   function. See [imputeLCMD::impute.MinProb()] for details and
##'   additional parameters.
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
##' imputation method arguments.
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
##' @aliases imputeMethods impute_neighbour_average impute_knn impute_mle impute_bpca impute_mixed impute_min impute_zero impute_with impute_matrix
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
##' @param x A matrix with missing values to be imputed.
##'
##' @param method `character(1)` defining the imputation method. See
##'     `imputeMethods()` for available ones.
##'
##' @param ... Additional parameters passed to the inner imputation
##'     function.
##'
##' @export
impute_matrix <- function(x,
                          method,
                          FUN,
                          ...) {
    ## stopifnot(is(m, "matrix"))
    if (!anyNA(x)) return(x)
    ## User-provided imputation function
    if (!missing(FUN) && is.function(FUN))
        return(impute_fun(x, FUN, ...))
    ## Function name provided as a character
    if (missing(method))
        stop("Please specify an imputation method. ",
             "See '?impute_matrix' for details.")
    method <- match.arg(method,
                        choices = imputeMethods(),
                        several.ok = FALSE)
    res <- x
    if (method %in% c("CRILC", "MinDet", "MinProb"))
        requireNamespace("imputeLCMD")
    if (method == "knn") {
        res <- impute_knn(x, ...)
    } else if (method == "nbavg") {
        res <- impute_neighbour_average(x, ...)
    } else if (method == "MLE") {
        res <- impute_mle(x, ...)
    } else if (method == "bpca"){
        res <- impute_bpca(x, ...)
    } else if (method == "QRILC") {
        res <- imputeLCMD::impute.QRILC(x, ...)[[1]]
    } else if (method == "MinDet") {
        res <- imputeLCMD::impute.MinDet(x, ...)
    } else if (method == "MinProb") {
        res <- imputeLCMD::impute.MinProb(x, ...)
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
impute_neighbour_average <- function(x, k = min(x, na.rm = TRUE)) {
    message("Assuming values are ordered.")
    .Call("C_impNeighbourAvg", x, k)
}

##' @export
##' @rdname imputation
impute_knn <- function(x, ...) {
    requireNamespace("impute")
    imp_res <- impute::impute.knn(x, ...)
    if (!is.null(imp_res$rng.state)) {
        assign(".Random.seed", imp_res$rng.state, envir = .GlobalEnv)
    } else {
        rm(".Random.seed", envir = .GlobalEnv)
    }
    imp_res$data
}

##' @export
##' @rdname imputation
impute_mle <- function(x, ...) {
    requireNamespace("norm")
    s <- norm::prelim.norm(x)  ## preliminary manipulations
    th <- norm::em.norm(s, ...) ## find the MLE
    seed <- sample(.Machine$integer.max, 1)
    norm::rngseed(seed) ## set random number generator seed
    norm::imp.norm(s, th, x)  ## impute missing data under the MLE
}

##' @export
##' @rdname imputation
impute_bpca <- function(x, ...) {
    requireNamespace("pcaMethods")
    nSamples <- dim(x)[2]
    .resultBPCA <- pcaMethods::pca(x,
                                   method = "bpca",
                                   nPcs = (nSamples - 1),
                                   verbose = FALSE,
                                   ...)
    pcaMethods::completeObs(.resultBPCA)
}

##' @export
##' @rdname imputation
impute_RF <- function(x, ...) {
    requireNamespace("missForest")
    t(missForest::missForest(t(x), ...)$ximp)
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
impute_mixed <- function(x, randna, mar, mnar, ...) {
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
    if (length(randna) != nrow(x))
        stop("Number of proteins and length of randna must be equal.",
             call. = FALSE)
    x[randna, ] <- impute_matrix(x[randna, ], mar, ...)
    x[!randna, ] <- impute_matrix(x[!randna, ], mnar, ...)
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
impute_fun <- function(x, FUN, ...) {
    res <- do.call(FUN, list(x, ...))
    stopifnot(identical(dim(x), dim(res)))
    res
}