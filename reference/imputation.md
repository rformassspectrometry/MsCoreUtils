# Quantitative mass spectrometry data imputation

The `impute_matrix` function performs data imputation on `matrix`
objects instance using a variety of methods (see below).

Users should proceed with care when imputing data and take precautions
to assure that the imputation produces valid results, in particular with
naive imputations such as replacing missing values with 0.

## Usage

``` r
impute_matrix(x, method, FUN, ...)

imputeMethods()

impute_neighbour_average(x, k = min(x, na.rm = TRUE), MARGIN = 1L)

impute_knn(x, MARGIN = 1L, ...)

impute_mle(x, MARGIN = 2L, ...)

impute_bpca(x, MARGIN = 1L, ...)

impute_RF(x, MARGIN = 2L, ...)

impute_mixed(x, randna, mar, mnar, MARGIN = 1L, ...)

impute_min(x)

impute_MinDet(x, q = 0.01, MARGIN = 2L)

impute_MinProb(x, q = 0.01, sigma = 1, MARGIN = 2L)

impute_QRILC(x, sigma = 1, MARGIN = 2L)

impute_zero(x)

impute_with(x, val)

impute_fun(x, FUN, MARGIN = 1L, ...)

getImputeMargin(fun)
```

## Arguments

- x:

  A matrix or an `HDF5Matrix` object to be imputed.

- method:

  `character(1)` defining the imputation method. See `imputeMethods()`
  for available ones.

- FUN:

  A user-provided function that takes a `matrix` as input and returns an
  imputed `matrix` of identical dimensions.

- ...:

  Additional parameters passed to the inner imputation function.

- k:

  `numeric(1)` providing the imputation value used for the first and
  last samples if they contain an `NA`. The default is to use the
  smallest value in the data.

- MARGIN:

  `integer(1)` defining the margin along which to apply imputation, with
  `1L` for rows and `2L` for columns. The default value will depend on
  the imputation method. Use `getImputeMargin(fun)` to get the default
  margin of imputation function `fun`. If the function doesn't take a
  margin argument, `NA` is returned.

- randna:

  `logical` of length equal to `nrow(object)` defining which rows are
  missing at random. The other ones are considered missing not at
  random. Only relevant when `methods` is `mixed`.

- mar:

  Imputation method for values missing at random. See `method` above.

- mnar:

  Imputation method for values missing not at random. See `method`
  above.

- q:

  `numeric(1)` indicating the quantile to be used to estimate the
  minimum in `MinDet` and `MinProb`. Default is 0.01.

- sigma:

  `numeric(1)` controling the standard deviation of the MNAR
  distribution in `MinProb` and `QRILC`. Default is 1.

- val:

  `numeric(1)` used to replace all missing values.

- fun:

  The imputation function to get the default margin from.

## Value

A matrix of same class as `x` with dimensions `dim(x)`.

## Types of missing values

There are two types of mechanisms resulting in missing values in LC/MSMS
experiments.

- Missing values resulting from absence of detection of a feature,
  despite ions being present at detectable concentrations. For example
  in the case of ion suppression or as a result from the stochastic,
  data-dependent nature of the DDA MS acquisition method. These missing
  value are expected to be randomly distributed in the data and are
  defined, in statistical terms, as missing at random (MAR) or missing
  completely at random (MCAR).

- Biologically relevant missing values resulting from the absence or the
  low abundance of ions (i.e. below the limit of detection of the
  instrument). These missing values are not expected to be randomly
  distributed in the data and are defined as missing not at random
  (MNAR).

MNAR features should ideally be imputed with a left-censor method, such
as `QRILC` below. Conversely, it is recommended to use hot deck methods
such nearest neighbours, Bayesian missing value imputation or maximum
likelihood methods when values are missing at random.

## Imputing by rows or columns

We assume that the input matrix `x` contains features along the rows and
samples along the columns, as is generally the case in omics data
analysis. When performing imputation, the missing values are taken as a
feature-specific property: feature *x* is missing because it is absent
(in a sample or group), or because it was missed during acquisition (not
selected during data dependent acquisition) or data processing (not
identified or with an identification score below a chosen false
discovery threshold). As such, imputation is by default performed at the
*feature level*. In some cases, such as imputation by zero or a global
minimum value, it doesn't matter. In other cases, it does matter very
much, such as for example when using the minimum value computed for each
margin (i.e. row or column) as in the *MinDet* method (see below) - do
we want to use the minimum of the sample or of that feature? KNN is
another such example: do we consider the most similar features to impute
a feature with missing values, or the most similar samples to impute all
missing in a sample.

The `MARGIN` argument can be used to change the imputation margin from
features/rows (`MARGIN = 1`) to samples/columns (`MARGIN = 2`).
Different imputations will have different default values, and changing
this parameter can have a major impact on imputation results and
downstream results.

## Imputation methods

Currently, the following imputation methods are available.

- *MLE*: Maximum likelihood-based imputation method using the EM
  algorithm. The `impute_mle()` function relies on
  [`norm::imp.norm()`](https://rdrr.io/pkg/norm/man/imp.norm.html).
  function. See
  [`norm::imp.norm()`](https://rdrr.io/pkg/norm/man/imp.norm.html) for
  details and additional parameters. Note that here, `...` are passed to
  the [`norm::em.norm()`](https://rdrr.io/pkg/norm/man/em.norm.html)
  function, rather to the actual imputation function `imp.norm`.

- *bpca*: Bayesian missing value imputation are available, as
  implemented in the
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html)
  function. See
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html) for
  details and additional parameters.

- *RF*: Random Forest imputation, as implemented in the
  [`missForest::missForest`](https://rdrr.io/pkg/missForest/man/missForest.html)
  function. See
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html)\]
  for details and additional parameters.

- *knn*: Nearest neighbour averaging, as implemented in the
  [`impute::impute.knn`](https://rdrr.io/pkg/impute/man/impute.knn.html)
  function. See
  [`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html)\]
  for details and additional parameters.

- *QRILC*: A missing data imputation method that performs the imputation
  of left-censored missing data using random draws from a truncated
  distribution with parameters estimated using quantile regression. The
  `impute_QRILC()` function calls
  [`imputeLCMD::impute.QRILC()`](https://rdrr.io/pkg/imputeLCMD/man/impute.QRILC.html)
  from the `imputeLCMD` package.

- *MinDet*: Performs the imputation of left-censored missing data using
  a deterministic minimal value approach. Considering a expression data
  with *n* samples and *p* features, for each sample, the missing
  entries are replaced with a minimal value observed in that sample. The
  minimal value observed is estimated as being the q-th quantile
  (default `q = 0.01`) of the observed values in that sample. The
  implementation in based on the
  [`imputeLCMD::impute.MinDet()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MinDet.html)
  function.

- *MinProb*: Performs the imputation of left-censored missing data by
  random draws from a Gaussian distribution centred to a minimal value.
  Considering an expression data matrix with *n* samples and *p*
  features, for each sample, the mean value of the Gaussian distribution
  is set to a minimal observed value in that sample. The minimal value
  observed is estimated as being the q-th quantile (default `q = 0.01`)
  of the observed values in that sample. The standard deviation is
  estimated as the median of the feature (or sample) standard
  deviations. Note that when estimating the standard deviation of the
  Gaussian distribution, only the peptides/proteins which present more
  than 50\\ values are considered. The `impute_MinProb()` function calls
  [`imputeLCMD::impute.MinProb()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MinProb.html)
  from the `imputeLCMD` package.

- *min*: Replaces the missing values with the smallest non-missing value
  in the data.

- *zero*: Replaces the missing values with 0.

- *mixed*: A mixed imputation applying two methods (to be defined by the
  user as `mar` for values missing at random and `mnar` for values
  missing not at random, see example) on two
  M[C](https://rdrr.io/r/stats/zC.html)AR/MNAR subsets of the data (as
  defined by the user by a `randna` logical, of length equal to
  nrow(object)).

- *nbavg*: Average neighbour imputation for fractions collected along a
  fractionation/separation gradient, such as sub-cellular fractions. The
  method assumes that the fraction are ordered along the gradient and is
  invalid otherwise.

  Continuous sets `NA` value at the beginning and the end of the
  quantitation vectors are set to the lowest observed value in the data
  or to a user defined value passed as argument `k`. Then, when a
  missing value is flanked by two non-missing neighbouring values, it is
  imputed by the mean of its direct neighbours.

- *with*: Replaces all missing values with a user-provided value.

- *none*: No imputation is performed and the missing values are left
  untouched. Implemented in case one wants to only impute value missing
  at random or not at random with the *mixed* method.

The `imputeMethods()` function returns a vector with valid imputation
method names. Use `getImputeMargin()` to get the default margin for each
imputation function.

## References

Olga Troyanskaya, Michael Cantor, Gavin Sherlock, Pat Brown, Trevor
Hastie, Robert Tibshirani, David Botstein and Russ B. Altman, Missing
value estimation methods for DNA microarrays Bioinformatics (2001) 17
(6): 520-525.

Oba et al., A Bayesian missing value estimation method for gene
expression profile data, Bioinformatics (2003) 19 (16): 2088-2096.

Cosmin Lazar (2015). imputeLCMD: A collection of methods for
left-censored missing data imputation. R package version 2.0.
<http://CRAN.R-project.org/package=imputeLCMD>.

Lazar C, Gatto L, Ferro M, Bruley C, Burger T. Accounting for the
Multiple Natures of Missing Values in Label-Free Quantitative Proteomics
Data Sets to Compare Imputation Strategies. J Proteome Res. 2016 Apr
1;15(4):1116-25. doi: 10.1021/acs.jproteome.5b00981. PubMed
PMID:26906401.

## Author

Laurent Gatto

## Examples

``` r

## test data
set.seed(42)
m <- matrix(rlnorm(60), 10)
dimnames(m) <- list(letters[1:10], LETTERS[1:6])
m[sample(60, 10)] <- NA

## available methods
imputeMethods()
#>  [1] "bpca"    "knn"     "QRILC"   "MLE"     "MLE2"    "MinDet"  "MinProb"
#>  [8] "min"     "zero"    "mixed"   "nbavg"   "with"    "RF"      "none"   

impute_matrix(m, method = "zero")
#>           A         B         C          D         E          F
#> a 3.9391243 3.6872085 0.0000000 1.57688302 1.2287515 1.37978165
#> b 0.5685317 9.8418666 0.1684176 0.00000000 0.6969391 0.45664959
#> c 1.4378205 0.2493592 0.8420488 2.81539768 0.0000000 4.83425736
#> d 1.8829931 0.7566997 3.3691979 0.54393454 0.0000000 0.00000000
#> e 1.4982059 0.8751838 6.6538355 1.65691116 0.2545441 1.09391242
#> f 0.8993127 0.0000000 0.6502040 0.17960259 1.5415957 1.31857387
#> g 4.5336257 0.0000000 0.7731599 0.45636653 0.4442387 1.97247444
#> h 0.9096830 0.0701966 0.1715015 0.42702719 4.2380415 1.09399145
#> i 0.0000000 0.0000000 0.0000000 0.08943818 0.6495690 0.05013228
#> j 0.9392120 3.7438457 0.5272951 1.03678296 1.9263902 1.32960639

impute_matrix(m, method = "min")
#>            A          B          C          D          E          F
#> a 3.93912433 3.68720845 0.05013228 1.57688302 1.22875148 1.37978165
#> b 0.56853172 9.84186664 0.16841764 0.05013228 0.69693906 0.45664959
#> c 1.43782048 0.24935924 0.84204876 2.81539768 0.05013228 4.83425736
#> d 1.88299314 0.75669973 3.36919788 0.54393454 0.05013228 0.05013228
#> e 1.49820590 0.87518382 6.65383554 1.65691116 0.25454413 1.09391242
#> f 0.89931266 0.05013228 0.65020399 0.17960259 1.54159566 1.31857387
#> g 4.53362571 0.05013228 0.77315991 0.45636653 0.44423873 1.97247444
#> h 0.90968305 0.07019660 0.17150153 0.42702719 4.23804154 1.09399145
#> i 0.05013228 0.05013228 0.05013228 0.08943818 0.64956901 0.05013228
#> j 0.93921196 3.74384570 0.52729513 1.03678296 1.92639019 1.32960639

impute_matrix(m, method = "knn")
#> Loading required namespace: impute
#> Imputing along margin 1 (features/rows).
#>           A         B         C          D         E          F
#> a 3.9391243 3.6872085 1.8777229 1.57688302 1.2287515 1.37978165
#> b 0.5685317 9.8418666 0.1684176 0.93096390 0.6969391 0.45664959
#> c 1.4378205 0.2493592 0.8420488 2.81539768 1.3352059 4.83425736
#> d 1.8829931 0.7566997 3.3691979 0.54393454 1.3352059 1.54557696
#> e 1.4982059 0.8751838 6.6538355 1.65691116 0.2545441 1.09391242
#> f 0.8993127 2.5204465 0.6502040 0.17960259 1.5415957 1.31857387
#> g 4.5336257 2.5204465 0.7731599 0.45636653 0.4442387 1.97247444
#> h 0.9096830 0.0701966 0.1715015 0.42702719 4.2380415 1.09399145
#> i 1.6608509 2.5204465 1.8777229 0.08943818 0.6495690 0.05013228
#> j 0.9392120 3.7438457 0.5272951 1.03678296 1.9263902 1.32960639

## same as impute_zero
impute_matrix(m, method = "with", val = 0)
#>           A         B         C          D         E          F
#> a 3.9391243 3.6872085 0.0000000 1.57688302 1.2287515 1.37978165
#> b 0.5685317 9.8418666 0.1684176 0.00000000 0.6969391 0.45664959
#> c 1.4378205 0.2493592 0.8420488 2.81539768 0.0000000 4.83425736
#> d 1.8829931 0.7566997 3.3691979 0.54393454 0.0000000 0.00000000
#> e 1.4982059 0.8751838 6.6538355 1.65691116 0.2545441 1.09391242
#> f 0.8993127 0.0000000 0.6502040 0.17960259 1.5415957 1.31857387
#> g 4.5336257 0.0000000 0.7731599 0.45636653 0.4442387 1.97247444
#> h 0.9096830 0.0701966 0.1715015 0.42702719 4.2380415 1.09399145
#> i 0.0000000 0.0000000 0.0000000 0.08943818 0.6495690 0.05013228
#> j 0.9392120 3.7438457 0.5272951 1.03678296 1.9263902 1.32960639

## impute with half of the smalles value
impute_matrix(m, method = "with",
              val = min(m, na.rm = TRUE) * 0.5)
#>            A          B          C          D          E          F
#> a 3.93912433 3.68720845 0.02506614 1.57688302 1.22875148 1.37978165
#> b 0.56853172 9.84186664 0.16841764 0.02506614 0.69693906 0.45664959
#> c 1.43782048 0.24935924 0.84204876 2.81539768 0.02506614 4.83425736
#> d 1.88299314 0.75669973 3.36919788 0.54393454 0.02506614 0.02506614
#> e 1.49820590 0.87518382 6.65383554 1.65691116 0.25454413 1.09391242
#> f 0.89931266 0.02506614 0.65020399 0.17960259 1.54159566 1.31857387
#> g 4.53362571 0.02506614 0.77315991 0.45636653 0.44423873 1.97247444
#> h 0.90968305 0.07019660 0.17150153 0.42702719 4.23804154 1.09399145
#> i 0.02506614 0.02506614 0.02506614 0.08943818 0.64956901 0.05013228
#> j 0.93921196 3.74384570 0.52729513 1.03678296 1.92639019 1.32960639

## all but third and fourth features' missing values
## are the result of random missing values
randna <- rep(TRUE, 10)
randna[c(3, 9)] <- FALSE

impute_matrix(m, method = "mixed",
              randna = randna,
              mar = "knn",
              mnar = "min")
#> Imputing along margin 1 (features/rows).
#>            A          B          C          D          E          F
#> a 3.93912433 3.68720845 2.35451487 1.57688302 1.22875148 1.37978165
#> b 0.56853172 9.84186664 0.16841764 0.74593934 0.69693906 0.45664959
#> c 1.43782048 0.24935924 0.84204876 2.81539768 0.05013228 4.83425736
#> d 1.88299314 0.75669973 3.36919788 0.54393454 1.34408497 1.44241604
#> e 1.49820590 0.87518382 6.65383554 1.65691116 0.25454413 1.09391242
#> f 0.89931266 2.42140409 0.65020399 0.17960259 1.54159566 1.31857387
#> g 4.53362571 2.42140409 0.77315991 0.45636653 0.44423873 1.97247444
#> h 0.90968305 0.07019660 0.17150153 0.42702719 4.23804154 1.09399145
#> i 0.05013228 0.05013228 0.05013228 0.08943818 0.64956901 0.05013228
#> j 0.93921196 3.74384570 0.52729513 1.03678296 1.92639019 1.32960639


## user provided (random) imputation function
random_imp <- function(x) {
   m <- mean(x, na.rm = TRUE)
   sdev <- sd(x, na.rm = TRUE)
   n <- sum(is.na(x))
   x[is.na(x)] <- rnorm(n, mean = m, sd = sdev)
   x
}

impute_matrix(m, FUN = random_imp)
#> Imputing along margin 1 (features/rows).
#>           A          B          C          D          E          F
#> a 3.9391243  3.6872085 -0.9827978 1.57688302  1.2287515 1.37978165
#> b 0.5685317  9.8418666  0.1684176 3.34043209  0.6969391 0.45664959
#> c 1.4378205  0.2493592  0.8420488 2.81539768  4.6343602 4.83425736
#> d 1.8829931  0.7566997  3.3691979 0.54393454 -0.3031275 0.79093969
#> e 1.4982059  0.8751838  6.6538355 1.65691116  0.2545441 1.09391242
#> f 0.8993127  2.0228995  0.6502040 0.17960259  1.5415957 1.31857387
#> g 4.5336257 -0.6151082  0.7731599 0.45636653  0.4442387 1.97247444
#> h 0.9096830  0.0701966  0.1715015 0.42702719  4.2380415 1.09399145
#> i 4.5173132  2.2355807  3.0879489 0.08943818  0.6495690 0.05013228
#> j 0.9392120  3.7438457  0.5272951 1.03678296  1.9263902 1.32960639

## get the default margin
getImputeMargin(impute_knn) ## default imputes along features
#> [1] 1

getImputeMargin(impute_mle) ## default imputes along samples
#> [1] 2

getImputeMargin(impute_zero) ## NA: no margin here
#> [1] NA

## default margin for all MsCoreUtils::impute_* functions
sapply(ls("package:MsCoreUtils", pattern = "impute_"), getImputeMargin)
#>            impute_MinDet           impute_MinProb             impute_QRILC 
#>                        2                        2                        2 
#>                impute_RF              impute_bpca               impute_fun 
#>                        2                        1                        1 
#>               impute_knn            impute_matrix               impute_min 
#>                        1                       NA                       NA 
#>             impute_mixed               impute_mle impute_neighbour_average 
#>                        1                        2                        1 
#>              impute_with              impute_zero 
#>                       NA                       NA 
```
