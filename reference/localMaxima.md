# Local Maxima

This function finds local maxima in a numeric vector. A local maximum is
defined as maximum in a window of the current index +/- `hws`.

## Usage

``` r
localMaxima(x, hws = 1L)
```

## Arguments

- x:

  `numeric`, vector that should be searched for local maxima.

- hws:

  `integer(1)`, half window size, the resulting window reaches from
  `(i - hws):(i + hws)`.

## Value

A `logical` of the same length as `x` that is `TRUE` for each local
maxima.

## See also

Other extreme value functions:
[`.peakRegionMask()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/peakRegionMask.md),
[`refineCentroids()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/refineCentroids.md),
[`valleys()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/valleys.md)

## Author

Sebastian Gibb

## Examples

``` r
x <- c(1:5, 4:1, 1:10, 9:1, 1:5, 4:1)
localMaxima(x)
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE
#> [37] FALSE
localMaxima(x, hws = 10)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [37] FALSE
```
