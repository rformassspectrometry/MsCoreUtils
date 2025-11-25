# Noise Estimation

This functions estimate the noise in the data.

## Usage

``` r
noise(x, y, method = c("MAD", "SuperSmoother"), ...)
```

## Arguments

- x:

  `numeric`, x values for noise estimation (e.g. *mz*)

- y:

  `numeric`, y values for noise estimation (e.g. intensity)

- method:

  `character(1)` used method. Currently MAD (median absolute deviation)
  and Friedman's SuperSmoother are supported.

- ...:

  further arguments passed to `method`.

## Value

A `numeric` of the same length as `x` with the estimated noise.

## See also

[`stats::mad()`](https://rdrr.io/r/stats/mad.html),
[`stats::supsmu()`](https://rdrr.io/r/stats/supsmu.html)

Other noise estimation and smoothing functions:
[`smooth()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/smooth.md)

## Author

Sebastian Gibb

## Examples

``` r
x <- 1:20
y <- c(1:10, 10:1)
noise(x, y)
#>  [1] 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065
#> [11] 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065 3.7065
noise(x, y, method = "SuperSmoother", span = 1 / 3)
#>  [1] 1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 7.857143
#>  [9] 8.428571 8.714286 8.714286 8.428571 7.857143 7.000000 6.000000 5.000000
#> [17] 4.000000 3.000000 2.000000 1.000000
```
