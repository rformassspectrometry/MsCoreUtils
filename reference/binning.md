# Binning

Aggregate values in `x` for bins defined on `y`: all values in `x` for
values in `y` falling into a bin (defined on `y`) are aggregated with
the provided function `FUN`.

## Usage

``` r
bin(
  x,
  y,
  size = 1,
  breaks = seq(floor(min(y)), ceiling(max(y)), by = size),
  FUN = max,
  returnMids = TRUE,
  .check = TRUE
)
```

## Arguments

- x:

  `numeric` with the values that should be aggregated/binned.

- y:

  `numeric` with same length than `x` with values to be used for the
  binning. `y` **must** be increasingly sorted, or else an error will be
  thrown.

- size:

  `numeric(1)` with the size of a bin.

- breaks:

  `numeric` defining the breaks (bins). See
  [`breaks_ppm()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/breaks_ppm.md)
  to define breaks with increasing size (depending on ppm).

- FUN:

  `function` to be used to aggregate values of `x` falling into the bins
  defined by `breaks`. `FUN` is expected to return a `numeric(1)`.

- returnMids:

  `logical(1)` whether the midpoints for the breaks should be returned
  in addition to the binned (aggregated) values of `x`. Setting
  `returnMids = FALSE` might be useful if the breaks are defined before
  hand and binning needs to be performed on a large set of values (i.e.
  within a loop for multiple pairs of `x` and `y` values).

- .check:

  `logical(1)` whether to check that `y` is an ordered vector. Setting
  `.check = FALSE` will improve performance, provided you are sure that
  `y` is always ordered.

## Value

Depending on the value of `returnMids`:

- `returnMids = TRUE` (the default): returns a `list` with elements `x`
  (aggregated values of `x`) and `mids` (the bin mid points).

- `returnMids = FALSE`: returns a `numeric` with just the binned values
  for `x`.

## See also

Other grouping/matching functions:
[`closest()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md),
[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)

## Author

Johannes Rainer, Sebastian Gibb

## Examples

``` r

## Define example intensities and m/z values
ints <- abs(rnorm(20, mean = 40))
mz <- seq(1:length(ints)) + rnorm(length(ints), sd = 0.001)

## Bin intensities by m/z bins with a bin size of 2
bin(ints, mz, size = 2)
#> $x
#>  [1] 37.56274 41.14841 39.75267 39.75580 39.44630 42.06502 40.51243 39.94740
#>  [9] 40.54300 40.46815
#> 
#> $mids
#>  [1]  1  3  5  7  9 11 13 15 17 19
#> 

## Repeat but summing up intensities instead of taking the max
bin(ints, mz, size = 2, FUN = sum)
#> $x
#>  [1]  37.56274 121.76439  77.93086  79.47309  39.44630  82.69401 117.01843
#>  [8]  79.42539  40.54300 119.91703
#> 
#> $mids
#>  [1]  1  3  5  7  9 11 13 15 17 19
#> 

## Get only the binned values without the bin mid points.
bin(ints, mz, size = 2, returnMids = FALSE)
#>  [1] 37.56274 41.14841 39.75267 39.75580 39.44630 42.06502 40.51243 39.94740
#>  [9] 40.54300 40.46815
```
