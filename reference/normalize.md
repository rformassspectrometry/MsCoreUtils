# Quantitative data normalisation

Function to normalise a matrix of quantitative omics data. The nature of
the normalisation is controlled by the `method` argument, described
below.

## Usage

``` r
normalizeMethods()

normalize_matrix(x, method, ...)
```

## Arguments

- x:

  A matrix or an `HDF5Matrix` object to be normalised.

- method:

  `character(1)` defining the normalisation method. See
  `normalizeMethods()` for available ones.

- ...:

  Additional parameters passed to the inner normalisation function.

## Value

A matrix of same class as `x` with dimensions `dim(x)`.

## Details

The `method` parameter can be one of `"sum"`, `"max"`, `"center.mean"`,
`"center.median"`, `"div.mean"`, `"div.median"`, `"diff.median"`,
`"quantiles`", `"quantiles.robust`" or `"vsn"`. The `normalizeMethods()`
function returns a vector of available normalisation methods.

- For `"sum"` and `"max"`, each feature's intensity is divided by the
  maximum or the sum of the feature respectively. These two methods are
  applied along the features (rows).

- `"center.mean"` and `"center.median"` center the respective sample
  (column) intensities by subtracting the respective column means or
  medians. `"div.mean"` and `"div.median"` divide by the column means or
  medians.

- `"diff.median"` centers all samples (columns) so that they all match
  the grand median by subtracting the respective columns medians
  differences to the grand median.

- Using `"quantiles"` or `"quantiles.robust"` applies (robust) quantile
  normalisation, as implemented in
  [`preprocessCore::normalize.quantiles()`](https://rdrr.io/pkg/preprocessCore/man/normalize.quantiles.html)
  and
  [`preprocessCore::normalize.quantiles.robust()`](https://rdrr.io/pkg/preprocessCore/man/normalize.quantiles.robust.html).
  `"vsn"` uses the
  [`vsn::vsn2()`](https://rdrr.io/pkg/vsn/man/vsn2.html) function. Note
  that the latter also glog-transforms the intensities. See respective
  manuals for more details and function arguments.

## See also

The [`scale()`](https://rdrr.io/r/base/scale.html) function that centers
(like `center.mean` above) and scales.

## Author

Laurent Gatto

## Examples

``` r
normalizeMethods()
#>  [1] "sum"              "max"              "center.mean"      "center.median"   
#>  [5] "div.mean"         "div.median"       "diff.median"      "quantiles"       
#>  [9] "quantiles.robust" "vsn"             

## test data
set.seed(42)
m <- matrix(rlnorm(60), 10)

normalize_matrix(m, method = "sum")
#>             [,1]        [,2]       [,3]        [,4]       [,5]        [,6]
#>  [1,] 0.31393285 0.293856133 0.05864968 0.125671426 0.09792670 0.109963217
#>  [2,] 0.04132996 0.715463967 0.01224328 0.147101553 0.05066466 0.033196581
#>  [3,] 0.11677032 0.020251317 0.06838566 0.228648076 0.17333805 0.392606576
#>  [4,] 0.21066540 0.084658010 0.37693894 0.060854278 0.05409294 0.212790434
#>  [5,] 0.12451231 0.072734432 0.55298434 0.137701921 0.02115455 0.090912443
#>  [6,] 0.13882341 0.291569272 0.10036947 0.027724557 0.23797015 0.203543139
#>  [7,] 0.50754608 0.084252023 0.08655639 0.051090905 0.04973318 0.220821420
#>  [8,] 0.13163892 0.010158049 0.02481774 0.061794488 0.61328088 0.158309924
#>  [9,] 0.75362945 0.008723409 0.15863000 0.008955514 0.06504185 0.005019784
#> [10,] 0.09883183 0.393959126 0.05548646 0.109099076 0.20271108 0.139912436

normalize_matrix(m, method = "max")
#>             [,1]       [,2]       [,3]       [,4]       [,5]        [,6]
#>  [1,] 1.00000000 0.93604775 0.18682236 0.40031309 0.31193519 0.350276239
#>  [2,] 0.05776666 1.00000000 0.01711237 0.20560302 0.07081371 0.046398677
#>  [3,] 0.29742324 0.05158171 0.17418369 0.58238473 0.44150573 1.000000000
#>  [4,] 0.55888470 0.22459344 1.00000000 0.16144333 0.14350584 0.564522299
#>  [5,] 0.22516425 0.13153073 1.00000000 0.24901595 0.03825525 0.164403285
#>  [6,] 0.47612497 1.00000000 0.34423885 0.09508737 0.81617020 0.698095301
#>  [7,] 1.00000000 0.16599877 0.17053898 0.10066259 0.09798752 0.435076596
#>  [8,] 0.21464703 0.01656345 0.04046717 0.10076050 1.00000000 0.258136084
#>  [9,] 1.00000000 0.01157520 0.21048806 0.01188318 0.08630481 0.006660813
#> [10,] 0.25086823 1.00000000 0.14084318 0.27692994 0.51454850 0.355144549

normalize_matrix(m, method = "quantiles")
#> Loading required namespace: preprocessCore
#>            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
#>  [1,] 2.4493083 2.4493083 0.9149807 1.5186864 1.1236151 1.5186864
#>  [2,] 0.2002101 5.9849751 0.2002101 2.9628356 0.9149807 0.3730709
#>  [3,] 0.9149807 0.6151294 1.5186864 5.9849751 2.9628356 5.9849751
#>  [4,] 1.5186864 0.9149807 2.9628356 0.9149807 0.6151294 2.4493083
#>  [5,] 1.1236151 1.1236151 5.9849751 2.4493083 0.2002101 0.6151294
#>  [6,] 0.3730709 1.5186864 0.7569865 0.3730709 1.5186864 0.9149807
#>  [7,] 2.9628356 0.7569865 1.1236151 0.7569865 0.3730709 2.9628356
#>  [8,] 0.6151294 0.2002101 0.3730709 0.6151294 5.9849751 0.7569865
#>  [9,] 5.9849751 0.3730709 2.4493083 0.2002101 0.7569865 0.2002101
#> [10,] 0.7569865 2.9628356 0.6151294 1.1236151 2.4493083 1.1236151

normalize_matrix(m, method = "center.mean")
#>             [,1]       [,2]       [,3]        [,4]       [,5]       [,6]
#>  [1,]  1.5256283  1.4919211 -0.8116640  0.49629688 -0.1310407 -0.1633550
#>  [2,] -1.8449643  7.6465793 -1.3791629  0.94293137 -0.6628531 -1.0864871
#>  [3,] -0.9756756 -1.9459281 -0.7055318  1.73481154  0.7745601  3.2911207
#>  [4,] -0.5305029 -1.4385876  1.8216174 -0.53665160 -0.8762926  0.3588507
#>  [5,] -0.9152902 -1.3201035  5.1062550  0.57632503 -1.1052480 -0.4492243
#>  [6,] -1.5141834 -0.3064709 -0.8973765 -0.90098354  0.1818035 -0.2245628
#>  [7,]  2.1201296 -1.4427110 -0.7744206 -0.62421961 -0.9155534  0.4293378
#>  [8,] -1.5038130 -2.1250907 -1.3760790 -0.65355895  2.8782494 -0.4491452
#>  [9,]  5.1129557 -2.1081671  0.0366477 -0.99114796 -0.7102232 -1.4930044
#> [10,] -1.4742841  1.5485584 -1.0202854 -0.04380318  0.5665980 -0.2135303
```
