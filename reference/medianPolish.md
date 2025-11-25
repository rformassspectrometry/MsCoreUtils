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
#>  [1] -0.7863926 -0.7754063 -0.3488269  0.4362125  0.3667570 -1.1307067
#>  [7]  0.1735711 -0.1450780 -0.9437531  0.6899024
```
