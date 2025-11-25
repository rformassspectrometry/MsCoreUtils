# Validation functions

These functions are used to validate input arguments. In general they
are just wrapper around their corresponding `is*` function with an error
message.

## Usage

``` r
validPeaksMatrix(x)
```

## Arguments

- x:

  object to test.

## Value

`logical(1)`, `TRUE` if validation are successful otherwise an error is
thrown.

## Details

`validPeaksMatrix`: see
[`isPeaksMatrix`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md).

## See also

Other helper functions for developers:
[`between()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/range.md),
[`isPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md),
[`rbindFill()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/rbindFill.md),
[`vapply1c()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/vapply.md),
[`which.first()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/which.md)

## Author

Sebastian Gibb

## Examples

``` r
try(validPeaksMatrix(1:2))
#> Error in validPeaksMatrix(1:2) : 
#>   'x' has to be a 'numeric' matrix with two columns named 'mz' and 'intensity'. The 'mz' column has to be sorted increasingly.
validPeaksMatrix(cbind(mz = 1:2, intensity = 1:2))
#> [1] TRUE
```
