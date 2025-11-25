# Check functions

These functions are used to check input arguments.

## Usage

``` r
isPeaksMatrix(x)
```

## Arguments

- x:

  object to test.

## Value

`logical(1)`, `TRUE` if checks are successful otherwise `FALSE`.

## Details

`isPeaksMatrix`: test for a `numeric` matrix with two columns named "mz"
and "intensity". The "mz" column has to be sorted increasingly.

## See also

Other helper functions for developers:
[`between()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/range.md),
[`rbindFill()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/rbindFill.md),
[`validPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/validation.md),
[`vapply1c()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/vapply.md),
[`which.first()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/which.md)

## Author

Sebastian Gibb

## Examples

``` r
isPeaksMatrix(1:2)
#> [1] FALSE
isPeaksMatrix(cbind(mz = 2:1, intensity = 1:2))
#> [1] FALSE
isPeaksMatrix(cbind(mz = 1:2, intensity = 1:2))
#> [1] TRUE
```
