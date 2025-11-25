# Forcing a numeric vector into a monotonously increasing sequence.

This function performs interpolation on the non-increasing parts of a
numeric input vector to ensure its values are monotonously increasing.
If the values are non-increasing at the end of the vector, these values
will be replaced by a sequence of numeric values, starting from the last
increasing value in the input vector, and increasing by a very small
value, which can be defined with parameter `by `

## Usage

``` r
force_sorted(x, by = .Machine$double.eps)
```

## Arguments

- x:

  `numeric` vector.

- by:

  `numeric(1)` value that will determine the monotonous increase in case
  the values at the end of the vector are non-increasing and therefore
  interpolation would not be possible. Defaults to
  `by = .Machine$double.eps` which is the smallest positive
  floating-point number x such that 1 + x != 1.

## Value

A vector with continuously increasing values.

## Note

NA values will not be replaced and be returned as-is.

## Examples

``` r
x <- c(NA, NA, NA, 1.2, 1.1, 1.14, 1.2, 1.3, NA, 1.04, 1.4, 1.6, NA, NA)
y <- force_sorted(x)
is.unsorted(y, na.rm = TRUE)
#> [1] FALSE

## Vector non increasing at the end
x <- c(1, 2, 1.5, 2)
y <- force_sorted(x, by = 0.1)
#> Warning: Found decreasing values at the end of the vector. Interpolation is not possible in this region. Instead, replacing these values with a sequence that starts from the last increasing value and increments by 0.1. See help for more details
is.unsorted(y, na.rm = TRUE)
#> [1] FALSE

## We can see the values were not interpolated but rather replaced by the
## last increasing value `2` and increasing by 0.1.
y
#> [1] 1.0 2.0 2.1 2.2
```
