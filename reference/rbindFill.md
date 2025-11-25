# Combine R Objects by Row

This function combines instances of `matrix`, `data.frame` or
`DataFrame` objects into a single instance adding eventually missing
columns (filling them with `NA`s).

## Usage

``` r
rbindFill(...)
```

## Arguments

- ...:

  2 or more: `matrix`, `data.frame` or `DataFrame`.

## Value

Depending on the input a single `matrix`, `data.frame` or `DataFrame`.

## Note

`rbindFill` might not work if one of the columns contains S4 classes.

## See also

Other helper functions for developers:
[`between()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/range.md),
[`isPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md),
[`validPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/validation.md),
[`vapply1c()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/vapply.md),
[`which.first()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/which.md)

## Author

Johannes Rainer, Sebastian Gibb

## Examples

``` r
## Combine matrices
a <- matrix(1:9, nrow = 3, ncol = 3)
colnames(a) <- c("a", "b", "c")
b <- matrix(1:12, nrow = 3, ncol = 4)
colnames(b) <- c("b", "a", "d", "e")
rbindFill(a, b)
#>      a b  c  d  e
#> [1,] 1 4  7 NA NA
#> [2,] 2 5  8 NA NA
#> [3,] 3 6  9 NA NA
#> [4,] 4 1 NA  7 10
#> [5,] 5 2 NA  8 11
#> [6,] 6 3 NA  9 12
rbindFill(b, a, b)
#>       b a  d  e  c
#>  [1,] 1 4  7 10 NA
#>  [2,] 2 5  8 11 NA
#>  [3,] 3 6  9 12 NA
#>  [4,] 4 1 NA NA  7
#>  [5,] 5 2 NA NA  8
#>  [6,] 6 3 NA NA  9
#>  [7,] 1 4  7 10 NA
#>  [8,] 2 5  8 11 NA
#>  [9,] 3 6  9 12 NA
```
