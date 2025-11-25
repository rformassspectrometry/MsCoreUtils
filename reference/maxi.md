# Maximum MS Intensity Value

`maxi` determines the maximum or mass spectrometry intensity values,
e.g. from a spectrum or chromatogram. In contrast to the base R
[`max()`](https://rdrr.io/r/base/Extremes.html) function this function
returns `NA_real_` if all intensity values are `NA` or if `length(x)` is
0 (the base R `max` function returns `-Inf` in these cases).

## Usage

``` r
maxi(x)
```

## Arguments

- x:

  `numeric` with intensity values from which the maximum should be
  reported. Will be coerced to `numeric`.

## Value

`numeric(1)` representing the maximum of values in `x`. Returns always a
`numeric` (double) even if `x` is an integer.

## See also

[`sumi()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/sumi.md)

## Author

Johannes Rainer, Sebastian Gibb

## Examples

``` r

x <- c(3.2, 34.4, 1.3, NA)
maxi(x)
#> [1] 34.4

## Compared to base R max:
max(x)
#> [1] NA
max(x, na.rm = TRUE)
#> [1] 34.4

max(numeric(), na.rm = TRUE)
#> Warning: no non-missing arguments to max; returning -Inf
#> [1] -Inf
maxi(numeric())
#> [1] NA

max(c(NA, NA), na.rm = TRUE)
#> Warning: no non-missing arguments to max; returning -Inf
#> [1] -Inf
maxi(c(NA, NA))
#> [1] NA
```
