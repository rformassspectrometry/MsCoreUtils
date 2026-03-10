# Return the Median Polish (Robust Twoway Decomposition) of a matrix

Fits an additive model (two way decomposition) using Tukey's median
polish procedure using
[`stats::medpolish()`](https://rdrr.io/r/stats/medpolish.html).

## Usage

``` r
medianPolish(x, verbose = FALSE, ...)
```

## Arguments

- x:

  A `matrix` of mode `numeric`.

- verbose:

  Default is `FALSE`.

- ...:

  Additional arguments passed to
  [`stats::medpolish()`](https://rdrr.io/r/stats/medpolish.html).

## Value

A `numeric` vector of length identical to `ncol(x)`.

## See also

Other Quantitative feature aggregation:
[`aggregate()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/aggregate.md),
[`colCounts()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/colCounts.md),
[`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)

## Author

Laurent Gatto

## Examples

``` r
x <- matrix(rnorm(30), nrow = 3)
medianPolish(x)
#>  [1] -0.26776266 -0.15710481 -0.75996920 -0.33338980  0.45164953  0.38219408
#>  [7] -0.55997060 -1.49835829  0.00120463 -0.03864196
```
