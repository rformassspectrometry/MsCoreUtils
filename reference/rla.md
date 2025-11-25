# Calculate relative log abundances

`rla` calculates the relative log abundances (RLA, see reference) on a
`numeric` vector. `rowRla` performs row-wise RLA calculations on a
numeric `matrix`.

## Usage

``` r
rla(
  x,
  f = rep_len(1, length(x)),
  transform = c("log2", "log10", "identity"),
  na.rm = TRUE
)

rowRla(x, f = rep_len(1, ncol(x)), transform = c("log2", "log10", "identity"))
```

## Arguments

- x:

  `numeric` (for `rla`) or `matrix` (for `rowRla`) with the abundances
  (in natural scale) on which the RLA should be calculated.

- f:

  `factor`, `numeric` or `character` with the same length than `x` (or,
  for `rowRla` equal to the number of columns of `x`) allowing to define
  the grouping of values in `x`. If omitted all values are considered to
  be from the same group.

- transform:

  `character(1)` defining the function to transform `x`. Defaults to
  `transform = "log2"` which `log2` transforms `x` prior to calculation.
  If `x` is already in log scale use `transform = "identity"` to avoid
  transformation of the values.

- na.rm:

  `logical(1)` whether `NA` values should be removed prior to
  calculation of the group-wise medians.

## Value

`numeric` with the relative log abundances (in log2 scale) with the same
length than `x` (for `rla`) or `matrix` with the same dimensions than
`x` (for `rowRla`).

## Details

The RLA is defined as the (log2) abundance of an analyte relative to the
median across all abundances of that analyte in samples of the same
group. The grouping of values can be defined with parameter `f`.

## References

De Livera AM, Dias DA, De Souza D, Rupasinghe T, Pyke J, Tull D,
Roessner U, McConville M, Speed TP. Normalizing and integrating
metabolomics data. *Anal Chem* 2012 Dec 18;84(24):10768-76.

## Author

Johannes Rainer

## Examples

``` r

x <- c(3, 4, 5, 1, 2, 3, 7, 8, 9)

grp <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

rla(x, grp)
#> [1] -0.4150375  0.0000000  0.3219281 -1.0000000  0.0000000  0.5849625 -0.1926451
#> [8]  0.0000000  0.1699250

x <- rbind(c(324, 4542, 3422, 3232, 5432, 6535, 3321, 1121),
     c(12, 3341, 3034, 6540, 34, 4532, 56, 1221))
grp <- c("a", "b", "b", "b", "a", "b", "a", "b")

## row-wise RLA values
rowRla(x, grp)
#>           [,1]      [,2]       [,3]        [,4]      [,5]      [,6]      [,7]
#> [1,] -3.357552 0.4084879  0.0000000 -0.08241256 0.7098658 0.9333475 0.0000000
#> [2,] -1.502500 0.0000000 -0.1390589  0.96901065 0.0000000 0.4398679 0.7198921
#>           [,8]
#> [1,] -1.610053
#> [2,] -1.452217
```
