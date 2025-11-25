# Grouping of numeric values by similarity

The `group` function groups numeric values by first ordering and then
putting all values into the same group if their difference is smaller
defined by parameters `tolerance` (a constant value) and `ppm` (a
value-specific relative value expressed in parts-per-million).

## Usage

``` r
group(x, tolerance = 0, ppm = 0)
```

## Arguments

- x:

  increasingly ordered `numeric` with the values to be grouped.

- tolerance:

  `numeric(1)` with the maximal accepted difference between values in
  `x` to be grouped into the same entity.

- ppm:

  `numeric(1)` defining a value-dependent maximal accepted difference
  between values in `x` expressed in parts-per-million.

## Value

`integer` of length equal to `x` with the groups.

## Note

Since grouping is performed on pairwise differences between consecutive
values (after ordering `x`), the difference between the smallest and
largest value in a group can be larger than `tolerance` and `ppm`.

## Author

Johannes Rainer, Sebastin Gibb

## Examples

``` r

## Define a (sorted) numeric vector
x <- c(34, 35, 35, 35 + ppm(35, 10), 56, 56.05, 56.1)

## With `ppm = 0` and `tolerance = 0` only identical values are grouped
group(x)
#> [1] 1 2 2 3 4 5 6

## With `tolerance = 0.05`
group(x, tolerance = 0.05)
#> [1] 1 2 2 2 3 3 3

## Also values 56, 56.05 and 56.1 were grouped into a single group,
## although the difference between the smallest 56 and largest value in
## this group (56.1) is 0.1. The (pairwise) difference between the ordered
## values is however 0.05.

## With ppm
group(x, ppm = 10)
#> [1] 1 2 2 2 3 4 5

## Same on an unsorted vector
x <- c(65, 34, 65.1, 35, 66, 65.2)
group(x, tolerance = 0.1)
#> [1] 3 1 3 2 4 3

## Values 65, 65.1 and 65.2 have been grouped into the same group.
```
