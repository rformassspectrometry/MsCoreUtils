# Relaxed Value Matching

These functions offer relaxed matching of one vector in another. In
contrast to the similar [`match()`](https://rdrr.io/r/base/match.html)
and [`%in%`](https://rdrr.io/r/base/match.html) functions they just
accept `numeric` arguments but have an additional `tolerance` argument
that allows relaxed matching.

## Usage

``` r
closest(
  x,
  table,
  tolerance = Inf,
  ppm = 0,
  duplicates = c("keep", "closest", "remove"),
  nomatch = NA_integer_,
  .check = TRUE
)

common(
  x,
  table,
  tolerance = Inf,
  ppm = 0,
  duplicates = c("keep", "closest", "remove"),
  .check = TRUE
)

join(
  x,
  y,
  tolerance = 0,
  ppm = 0,
  type = c("outer", "left", "right", "inner"),
  .check = TRUE,
  ...
)
```

## Arguments

- x:

  `numeric`, the values to be matched. In contrast to
  [`match()`](https://rdrr.io/r/base/match.html) `x` has to be sorted in
  increasing order and must not contain any `NA`.

- table:

  `numeric`, the values to be matched against. In contrast to
  [`match()`](https://rdrr.io/r/base/match.html) `table` has to be
  sorted in increasing order and must not contain any `NA`.

- tolerance:

  `numeric`, accepted tolerance. Could be of length one or the same
  length as `x`.

- ppm:

  `numeric(1)` representing a relative, value-specific parts-per-million
  (PPM) tolerance that is added to `tolerance`.

- duplicates:

  `character(1)`, how to handle duplicated matches. Has to be one of
  `c("keep", "closest", "remove")`. No abbreviations allowed.

- nomatch:

  `integer(1)`, if the difference between the value in `x` and `table`
  is larger than `tolerance` `nomatch` is returned.

- .check:

  `logical(1)` turn off checks for increasingly sorted `x` and `y`. This
  should just be done if it is ensured by other methods that `x` and `y`
  are sorted, see also `closest()`.

- y:

  `numeric`, the values to be joined. Should be sorted.

- type:

  `character(1)`, defines how `x` and `y` should be joined. See details
  for `join`.

- ...:

  ignored.

## Value

`closest` returns an `integer` vector of the same length as `x` giving
the closest position in `table` of the first match or `nomatch` if there
is no match.

`common` returns a `logical` vector of length `x` that is `TRUE` if the
element in `x` was found in `table`. It is similar to
[`%in%`](https://rdrr.io/r/base/match.html).

`join` returns a `matrix` with two columns, namely `x` and `y`,
representing the index of the values in `x` matching the corresponding
value in `y` (or `NA` if the value does not match).

## Details

For `closest`/`common` the `tolerance` argument could be set to `0` to
get the same results as for
[`match()`](https://rdrr.io/r/base/match.html)/[`%in%`](https://rdrr.io/r/base/match.html).
If it is set to `Inf` (default) the index of the closest values is
returned without any restriction.

It is not guaranteed that there is a one-to-one matching for neither the
`x` to `table` nor the `table` to `x` matching.

If multiple elements in `x` match a single element in `table` all their
corresponding indices are returned if `duplicates="keep"` is set
(default). This behaviour is identical to
[`match()`](https://rdrr.io/r/base/match.html). For
`duplicates="closest"` just the closest element in `x` gets the
corresponding index in `table` and for `duplicates="remove"` all
elements in `x` that match to the same element in `table` are set to
`nomatch`.

If a single element in `x` matches multiple elements in `table` the
*closest* is returned for `duplicates="keep"` or `duplicates="closest"`
(*keeping* multiple matches isn't possible in this case because the
return value should be of the same length as `x`). If the differences
between `x` and the corresponding matches in `table` are identical the
lower index (the smaller element in `table`) is returned. There is one
exception: if the lower index is already returned for another `x` with a
smaller difference to this `index` the higher one is returned for
`duplicates = "closer"` (but only if there is no other `x` that is
closer to the higher one). For `duplicates="remove"` all multiple
matches are returned as `nomatch` as above.

`.checks = TRUE` tests among other input validation checks for
increasingly sorted `x` and `table` arguments that are mandatory
assumptions for the `closest` algorithm. These checks require to loop
through both vectors and compare each element against its precursor.
Depending on the length and distribution of `x` and `table` these checks
take equal/more time than the whole `closest` algorithm. If it is
ensured by other methods that both arguments `x` and `table` are sorted
the tests could be skipped by `.check = FALSE`. In the case that
`.check = FALSE` is used and one of `x` and `table` is not sorted (or
decreasingly sorted) the output would be incorrect in the best case and
result in infinity loop in the average and worst case.

`join`: joins two `numeric` vectors by mapping values in `x` with values
in `y` and *vice versa* if they are similar enough (provided the
`tolerance` and `ppm` specified). The function returns a `matrix` with
the indices of mapped values in `x` and `y`. Parameter `type` allows to
define how the vectors will be joined: `type = "left"`: values in `x`
will be mapped to values in `y`, elements in `y` not matching any value
in `x` will be discarded. `type = "right"`: same as `type = "left"` but
for `y`. `type = "outer"`: return matches for all values in `x` and in
`y`. `type = "inner"`: report only indices of values that could be
mapped.

## Note

`join` is based on `closest(x, y, tolerance, duplicates = "closest")`.
That means for multiple matches just the closest one is reported.

## See also

[`match()`](https://rdrr.io/r/base/match.html)

[`%in%`](https://rdrr.io/r/base/match.html)

Other grouping/matching functions:
[`bin()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/binning.md),
[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)

## Author

Sebastian Gibb, Johannes Rainer

## Examples

``` r
## Define two vectors to match
x <- c(1, 3, 5)
y <- 1:10

## Compare match and closest
match(x, y)
#> [1] 1 3 5
closest(x, y)
#> [1] 1 3 5

## If there is no exact match
x <- x + 0.1
match(x, y) # no match
#> [1] NA NA NA
closest(x, y)
#> [1] 1 3 5

## Some new values
x <- c(1.11, 45.02, 556.45)
y <- c(3.01, 34.12, 45.021, 46.1, 556.449)

## Using a single tolerance value
closest(x, y, tolerance = 0.01)
#> [1] NA  3  5

## Using a value-specific tolerance accepting differences of 20 ppm
closest(x, y, ppm = 20)
#> [1] 1 3 5

## Same using 50 ppm
closest(x, y, ppm = 50)
#> [1] 1 3 5

## Sometimes multiple elements in `x` match to `table`
x <- c(1.6, 1.75, 1.8)
y <- 1:2
closest(x, y, tolerance = 0.5)
#> [1] 2 2 2
closest(x, y, tolerance = 0.5, duplicates = "closest")
#> [1] NA NA  2
closest(x, y, tolerance = 0.5, duplicates = "remove")
#> [1] NA NA NA

## Are there any common values?
x <- c(1.6, 1.75, 1.8)
y <- 1:2
common(x, y, tolerance = 0.5)
#> [1] TRUE TRUE TRUE
common(x, y, tolerance = 0.5, duplicates = "closest")
#> [1] FALSE FALSE  TRUE
common(x, y, tolerance = 0.5, duplicates = "remove")
#> [1] FALSE FALSE FALSE

## Join two vectors
x <- c(1, 2, 3, 6)
y <- c(3, 4, 5, 6, 7)

jo <- join(x, y, type = "outer")
jo
#> $x
#> [1]  1  2  3 NA NA  4 NA
#> 
#> $y
#> [1] NA NA  1  2  3  4  5
#> 
x[jo$x]
#> [1]  1  2  3 NA NA  6 NA
y[jo$y]
#> [1] NA NA  3  4  5  6  7

jl <- join(x, y, type = "left")
jl
#> $x
#> [1] 1 2 3 4
#> 
#> $y
#> [1] NA NA  1  4
#> 
x[jl$x]
#> [1] 1 2 3 6
y[jl$y]
#> [1] NA NA  3  6

jr <- join(x, y, type = "right")
jr
#> $x
#> [1]  3 NA NA  4 NA
#> 
#> $y
#> [1] 1 2 3 4 5
#> 
x[jr$x]
#> [1]  3 NA NA  6 NA
y[jr$y]
#> [1] 3 4 5 6 7

ji <- join(x, y, type = "inner")
ji
#> $x
#> [1] 3 4
#> 
#> $y
#> [1] 1 4
#> 
x[ji$x]
#> [1] 3 6
y[ji$y]
#> [1] 3 6
```
