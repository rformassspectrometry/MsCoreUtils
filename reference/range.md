# Range helper functions

These functions help to work with `numeric` ranges.

## Usage

``` r
between(x, range)

x %between% range
```

## Arguments

- x:

  `numeric`, input values.

- range:

  `numeric(2)`, range to compare against.

## Value

`logical` vector of length `length(x)`.

## See also

Other helper functions for developers:
[`isPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md),
[`rbindFill()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/rbindFill.md),
[`validPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/validation.md),
[`vapply1c()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/vapply.md),
[`which.first()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/which.md)

## Author

Sebastian Gibb

## Examples

``` r
between(1:4, 2:3)
#> [1] FALSE  TRUE  TRUE FALSE
1:4 %between% 2:3
#> [1] FALSE  TRUE  TRUE FALSE
```
