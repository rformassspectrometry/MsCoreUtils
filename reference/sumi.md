# Summing MS Intensity Values

`sumi` sums mass spectrometry intensity values, e.g. from a spectrum or
chromatogram. In contrast to the base R
[`sum()`](https://rdrr.io/r/base/sum.html) function this function
returns `NA_real_` if all intensity values are `NA` or if `length(x)` is
0.

## Usage

``` r
sumi(x)
```

## Arguments

- x:

  `numeric` with intensity values to be summed up. Will be coerced to
  `numeric` using `as.double`.

## Value

`numeric(1)` representing the sum of values in `x`. Always returns a
numeric (double) even if `x` is an integer.

## See also

[`maxi()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/maxi.md)

## Author

Johannes Rainer

## Examples

``` r

x <- c(3.2, 34.4, 1.3, NA)
sumi(x)
#> [1] 38.9

## Compared to base R sum:
sum(x)
#> [1] NA
sum(x, na.rm = TRUE)
#> [1] 38.9

sum(numeric(), na.rm = TRUE)
#> [1] 0
sumi(numeric())
#> [1] NA

sum(c(NA, NA), na.rm = TRUE)
#> [1] 0
sumi(c(NA, NA))
#> [1] NA
```
