# Return the Robust Expression Summary of a matrix

This function calculates the robust summarisation for each feature
(protein). Note that the function assumes that the intensities in input
`e` are already log-transformed.

## Usage

``` r
robustSummary(x, ...)
```

## Arguments

- x:

  A feature by sample `matrix` containing quantitative data with
  mandatory `colnames` and `rownames`.

- ...:

  Additional arguments passed to
  [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html).

## Value

[`numeric()`](https://rdrr.io/r/base/numeric.html) vector of length
`ncol(x)` with robust summarised values.

## See also

Other Quantitative feature aggregation:
[`aggregate()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/aggregate.md),
[`colCounts()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/colCounts.md),
[`medianPolish()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/medianPolish.md)

## Author

Adriaan Sticker, Sebastian Gibb and Laurent Gatto

## Examples

``` r
x <- matrix(rnorm(30), nrow = 3)
colnames(x) <- letters[1:10]
rownames(x) <- LETTERS[1:3]
robustSummary(x)
#>           a           b           c           d           e           f 
#>  0.13327322  0.91145468  0.76502760 -0.23239400 -0.41392131  0.60431427 
#>           g           h           i           j 
#> -0.51182415  0.07515504 -0.39439300  0.52412091 
```
