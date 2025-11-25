# Counts the number of features

Returns the number of non-NA features in a features by sample matrix.

## Usage

``` r
colCounts(x, ...)
```

## Arguments

- x:

  A `matrix` of mode `numeric`.

- ...:

  Currently ignored.

## Value

A `numeric` vector of length identical to `ncol(x)`.

## See also

Other Quantitative feature aggregation:
[`aggregate()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/aggregate.md),
[`medianPolish()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/medianPolish.md),
[`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)

## Author

Laurent Gatto

## Examples

``` r
m <- matrix(c(1, NA, 2, 3, NA, NA, 4, 5, 6),
            nrow = 3)
colCounts(m)
#> [1] 2 1 3
m <- matrix(rnorm(30), nrow = 3)
colCounts(m)
#>  [1] 3 3 3 3 3 3 3 3 3 3
```
