# Grouping of numeric values by similarity of m/z and intensity

The `group_mz_int()` function groups peaks with similar m/z (across
several scans) considering also their intensity. The algorithm first
orders peak decreasingly by their intensity. Then it iteratively selects
the peak with the highest intensity that is not yet part of a peak group
and finds all other peaks with a difference in their m/z that is smaller
than defined by `tolerance` and `ppm`. These peaks are assigned to the
same peak group.

Setting parameter `max_num` to a finite number forces each peak group to
contain only the at most `max_num` peaks ordered by their intensity.

## Usage

``` r
group_mz_int(x, y = numeric(), max_num = Inf, tolerance = 0, ppm = 0)
```

## Arguments

- x:

  `numeric` with the *m/z* values to be grouped.

- y:

  `numeric` with the intensity values of the peaks.

- max_num:

  `integer(1)` defining the maximum number of peaks for a peak group.

- tolerance:

  `numeric(1)` with the maximal accepted difference between values in
  `x` to be grouped into the same entity.

- ppm:

  `numeric(1)` defining a value-dependent maximal accepted difference
  between values in `x` expressed in parts-per-million.

## Value

`integer` of length equal to `x` with the groups.

## Note

This method solves the scenario like the difference between the smallest
and largest value in a group can be larger than `tolerance` and `ppm`.

## See also

[`group()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/group.md)

## Author

Muyao Xi

## Examples

``` r

## Define a (sorted) numeric vector
x = c(56, 56.004, 56.008, 56.012, 56.016, 56.02)
y = c(52151, 125584, 582, 58452, 458, 57452)
max_num = 2

## With `ppm = 0` and `tolerance = 0` only identical values are grouped
group_mz_int(x, y, max_num)
#> [1] 4 1 5 2 6 3

## With `tolerance = 0.005`
group_mz_int(x, y, max_num, tolerance = 0.005)
#> [1] 1 1 2 2 3 3

## three groups were made.

## With ppm
group_mz_int(x, y, max_num, ppm = 10)
#> [1] 4 1 5 2 6 3

## Same on an unsorted vector
x <- c(56, 56.012, 56.016, 56.004, 56.008, 56.02)
y = c(52151, 58452, 458, 125584, 582, 57452)
group_mz_int(x, y, max_num, tolerance = 0.005)
#> [1] 1 2 3 1 2 3

## the same three groups were made.
```
