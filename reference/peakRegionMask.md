# Peak Region Mask

This function finds the mz region spanning by a peak. It creates an 0/1
matrix used for multiplications in other functions.

## Usage

``` r
.peakRegionMask(x, p, k = 30L)
```

## Arguments

- x:

  `numeric`, e.g. intensity values.

- p:

  `integer`, indices of identified peaks/local maxima.

- k:

  `integer(1)`: maximum number of values left and right of the peak that
  should be looked for valleys.

## Value

A `matrix` with a column for each peak in `p` and `2 * k + 1` rows where
the middle row `k + 1` is the peak centroid. If the values is `1` the
index belongs to the peak region.

## See also

Other extreme value functions:
[`localMaxima()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/localMaxima.md),
[`refineCentroids()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/refineCentroids.md),
[`valleys()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/valleys.md)

## Author

Sebastian Gibb

## Examples

``` r
ints <- c(5, 8, 12, 7, 4, 9, 15, 16, 11, 8, 3, 2, 3, 2, 9, 12, 14, 13, 8, 3)
mzs <- seq_along(ints)
peaks <- which(localMaxima(ints, hws = 3L))

m <- MsCoreUtils:::.peakRegionMask(ints, peaks, k = 5L)
```
