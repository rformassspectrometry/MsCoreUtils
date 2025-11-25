# Reduce overlapping numeric ranges to disjoined ranges

The `reduce()` function *reduces* the provided numeric ranges to
non-overlapping (disjoint) ranges. This is similar to the
[`IRanges::reduce()`](https://rdrr.io/pkg/IRanges/man/inter-range-methods.html)
function, but works with `numeric` vectors instead of integer ranges
(`IRanges`).

## Usage

``` r
reduce(start = numeric(), end = numeric(), .check = TRUE)
```

## Arguments

- start:

  `numeric` with the lower (start) values for each numeric range.

- end:

  `numeric` with the upper (end) values for each numeric range. Has to
  match the length of `start` and `all(start <= end)` has to be `TRUE`.

- .check:

  `logical(1)` whether input parameter validations should be performed.
  With `.check = TRUE` (the default) the function checks if the length
  of input parameters `start` and `end` is the same and whether all
  values in `start` are `<=` the values in `end`.

## Value

`list` of length 2, the first element being the start (mininum) values
for the disjoint ranges, the second the end (maximum) values.

## Note

The *IRanges* package defines a `reduce()` method for `IRanges` and
other S4 classes. This `reduce()` function is not an S4 method, but a
function, thus it is suggested to specifically import it if used in
another R package, or to call it with `MsCoreUtils::reduce()`.

## Author

Johannes Rainer and Sebastian Gibb

## Examples

``` r

## Define start and end values for the numeric ranges
s <- c(12.23, 21.2, 13.4, 14.2, 15.0, 43.12)
e <- c(12.40, 24.1, 14.4, 16.2, 15.2, 55.23)

reduce(s, e)
#> [[1]]
#> [1] 12.23 13.40 21.20 43.12
#> 
#> [[2]]
#> [1] 12.40 16.20 24.10 55.23
#> 

## Empty vectors
reduce()
#> [[1]]
#> numeric(0)
#> 
#> [[2]]
#> numeric(0)
#> 

## Single value
reduce(3.12, 34)
#> [[1]]
#> [1] 3.12
#> 
#> [[2]]
#> [1] 34
#> 

## Non-overlapping ranges
reduce(c(3, 9), c(4, 19))
#> [[1]]
#> [1] 3 9
#> 
#> [[2]]
#> [1]  4 19
#> 
```
