# vapply wrappers

These functions are short wrappers around typical `vapply` calls for
easier development.

## Usage

``` r
vapply1c(X, FUN, ..., USE.NAMES = FALSE)

vapply1d(X, FUN, ..., USE.NAMES = FALSE)

vapply1l(X, FUN, ..., USE.NAMES = FALSE)
```

## Arguments

- X:

  a vector (atomic or `list`).

- FUN:

  the `function` to be applied to each element of `X`.

- ...:

  optional arguments to `FUN`.

- USE.NAMES:

  `logical`, should the return value be named.

## Value

`vapply1c` returns a vector of `character`s of length `X`.

`vapply1d` returns a vector of `double`s of length `X`.

`vapply1l` returns a vector of `logical`s of length `X`.

## See also

Other helper functions for developers:
[`between()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/range.md),
[`isPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/check.md),
[`rbindFill()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/rbindFill.md),
[`validPeaksMatrix()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/validation.md),
[`which.first()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/which.md)

## Author

Sebastian Gibb

## Examples

``` r
l <- list(a=1:3, b=4:6)
vapply1d(l, sum)
#> [1]  6 15
```
