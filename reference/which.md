# Which is the first/last TRUE value.

Determines the location, i.e., index of the first or last `TRUE` value
in a logical vector.

## Usage

``` r
which.first(x)

which.last(x)
```

## Arguments

- x:

  `logical`, vector.

## Value

`integer`, index of the first/last `TRUE` value. `integer(0)` if no
`TRUE` (everything `FALSE` or `NA`) was found.

## See also

[`which.min()`](https://rdrr.io/r/base/which.min.html)

Other helper functions for developers:
[`between()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/range.md),
[`isPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md),
[`rbindFill()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/rbindFill.md),
[`validPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/validation.md),
[`vapply1c()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/vapply.md)

## Author

Sebastian Gibb

## Examples

``` r
l <- 2 <= 1:3
which.first(l)
#> [1] 2
which.last(l)
#> [1] 3
```
