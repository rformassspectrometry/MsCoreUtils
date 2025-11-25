# Aggreagate quantitative features

These functions take a matrix of quantitative features `x` and aggregate
the features (rows) according to either a vector (or factor) `INDEX` or
an adjacency matrix `MAT`. The aggregation method is defined by function
`FUN`.

Adjacency matrices are an elegant way to explicitly encode for shared
peptides (see example below) during aggregation.

## Usage

``` r
colMeansMat(x, MAT, na.rm = FALSE)

colSumsMat(x, MAT, na.rm = FALSE)

aggregate_by_matrix(x, MAT, FUN, ...)

aggregate_by_vector(x, INDEX, FUN, ...)
```

## Arguments

- x:

  A `matrix` of mode `numeric` or an `HDF5Matrix` object of type
  `numeric`.

- MAT:

  An adjacency matrix that defines peptide-protein relations with
  `nrow(MAT) == nrow(x)`: a non-missing/non-null value at position (i,j)
  indicates that peptide i belong to protein j. This matrix is tyically
  binary but can also contain weighted relations.

- na.rm:

  A `logical(1)` indicating whether the missing values (including NaN)
  should be omitted from the calculations or not. Defaults to `FALSE`.

- FUN:

  A `function` to be applied to the subsets of `x`.

- ...:

  Additional arguments passed to `FUN`.

- INDEX:

  A `vector` or `factor` of length `nrow(x)`.

## Value

`aggregate_by_matrix()` returns a `matrix` (or `Matrix`) of dimensions
`ncol(MAT)` and
`ncol(x), with `dimnames`equal to`colnames(x)`and`rownames(MAT)\`.

`aggregate_by_vector()` returns a new `matrix` (if `x` is a `matrix`) or
`HDF5Matrix` (if `x` is an `HDF5Matrix`) of dimensions `length(INDEX)`
and `ncol(x), with `dimnames` equal to`colnames(x)`and`INDEX\`.

## Vector-based aggregation functions

When aggregating with a vector/factor, user-defined functions must
return a vector of length equal to `ncol(x)` for each level in `INDEX`.
Examples thereof are:

- [`medianPolish()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/medianPolish.md)
  to fits an additive model (two way decomposition) using Tukey's median
  polish procedure using
  [`stats::medpolish()`](https://rdrr.io/r/stats/medpolish.html);

- [`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)
  to calculate a robust aggregation using
  [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html);

- [`base::colMeans()`](https://rdrr.io/r/base/colSums.html) to use the
  mean of each column;

- [`base::colSums()`](https://rdrr.io/r/base/colSums.html) to use the
  sum of each column;

- [`matrixStats::colMedians()`](https://rdrr.io/pkg/matrixStats/man/rowMedians.html)
  to use the median of each column.

## Matrix-based aggregation functions

When aggregating with an adjacency matrix, user-defined functions must
return a new matrix. Examples thereof are:

- `colSumsMat(x, MAT)` aggregates by the summing the peptide intensities
  for each protein. Shared peptides are re-used multiple times.

- `colMeansMat(x, MAT)` aggregation by the calculating the mean of
  peptide intensities. Shared peptides are re-used multiple times.

## Handling missing values

By default, missing values in the quantitative data will propagate to
the aggregated data. You can provide `na.rm = TRUE` to most functions
listed above to ignore missing values, except for
[`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)
where you should supply `na.action = na.omit` (see
[`?MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html)).

## See also

Other Quantitative feature aggregation:
[`colCounts()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/colCounts.md),
[`medianPolish()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/medianPolish.md),
[`robustSummary()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/robustSummary.md)

## Author

Laurent Gatto and Samuel Wieczorek (aggregation from an adjacency
matrix).

## Examples

``` r

x <- matrix(c(10.39, 17.16, 14.10, 12.85, 10.63, 7.52, 3.91,
              11.13, 16.53, 14.17, 11.94, 11.51, 7.69, 3.97,
              11.93, 15.37, 14.24, 11.21, 12.29, 9.00, 3.83,
              12.90, 14.37, 14.16, 10.12, 13.33, 9.75, 3.81),
            nrow = 7,
            dimnames = list(paste0("Pep", 1:7), paste0("Sample", 1:4)))
x
#>      Sample1 Sample2 Sample3 Sample4
#> Pep1   10.39   11.13   11.93   12.90
#> Pep2   17.16   16.53   15.37   14.37
#> Pep3   14.10   14.17   14.24   14.16
#> Pep4   12.85   11.94   11.21   10.12
#> Pep5   10.63   11.51   12.29   13.33
#> Pep6    7.52    7.69    9.00    9.75
#> Pep7    3.91    3.97    3.83    3.81

## -------------------------
## Aggregation by vector
## -------------------------

(k <- paste0("Prot", c("B", "E", "X", "E", "B", "B", "E")))
#> [1] "ProtB" "ProtE" "ProtX" "ProtE" "ProtB" "ProtB" "ProtE"

aggregate_by_vector(x, k, colMeans)
#>         Sample1  Sample2  Sample3   Sample4
#> ProtB  9.513333 10.11000 11.07333 11.993333
#> ProtE 11.306667 10.81333 10.13667  9.433333
#> ProtX 14.100000 14.17000 14.24000 14.160000
aggregate_by_vector(x, k, robustSummary)
#>         Sample1  Sample2  Sample3  Sample4
#> ProtB  9.513333 10.15800 11.07333 11.99333
#> ProtE 11.451493 10.81333 10.13667  9.22360
#> ProtX 14.100000 14.17000 14.24000 14.16000
aggregate_by_vector(x, k, medianPolish)
#>       Sample1 Sample2 Sample3 Sample4
#> ProtB   10.39   11.13   11.93   12.90
#> ProtE   12.85   11.94   11.21   10.12
#> ProtX   14.10   14.17   14.24   14.16

## -------------------------
## Aggregation by matrix
## -------------------------

adj <- matrix(c(1, 0, 0, 1, 1, 1, 0, 0,
                1, 0, 1, 0, 0, 1, 0, 0,
                1, 0, 0, 0, 1),
              nrow = 7,
              dimnames = list(paste0("Pep", 1:7),
                              paste0("Prot", c("B", "E", "X"))))
adj
#>      ProtB ProtE ProtX
#> Pep1     1     0     0
#> Pep2     0     1     0
#> Pep3     0     0     1
#> Pep4     1     1     0
#> Pep5     1     0     0
#> Pep6     1     0     0
#> Pep7     0     1     1

## Peptide 4 is shared by 2 proteins (has a rowSums of 2),
## namely proteins B and E
rowSums(adj)
#> Pep1 Pep2 Pep3 Pep4 Pep5 Pep6 Pep7 
#>    1    1    1    2    1    1    2 

aggregate_by_matrix(x, adj, colSumsMat)
#>       Sample1 Sample2 Sample3 Sample4
#> ProtB   41.39   42.27   44.43   46.10
#> ProtE   33.92   32.44   30.41   28.30
#> ProtX   18.01   18.14   18.07   17.97
aggregate_by_matrix(x, adj, colMeansMat)
#>        Sample1  Sample2  Sample3   Sample4
#> ProtB 10.34750 10.56750 11.10750 11.525000
#> ProtE 11.30667 10.81333 10.13667  9.433333
#> ProtX  9.00500  9.07000  9.03500  8.985000

## ---------------
## Missing values
## ---------------

x <- matrix(c(NA, 2:6), ncol = 2,
            dimnames = list(paste0("Pep", 1:3),
                            c("S1", "S2")))
x
#>      S1 S2
#> Pep1 NA  4
#> Pep2  2  5
#> Pep3  3  6

## simply use na.rm = TRUE to ignore missing values
## during the aggregation

(k <- LETTERS[c(1, 1, 2)])
#> [1] "A" "A" "B"
aggregate_by_vector(x, k, colSums)
#>   S1 S2
#> A NA  9
#> B  3  6
aggregate_by_vector(x, k, colSums, na.rm = TRUE)
#>   S1 S2
#> A  2  9
#> B  3  6

(adj <- matrix(c(1, 1, 0, 0, 0, 1), ncol = 2,
               dimnames = list(paste0("Pep", 1:3),
                           c("A", "B"))))
#>      A B
#> Pep1 1 0
#> Pep2 1 0
#> Pep3 0 1
aggregate_by_matrix(x, adj, colSumsMat, na.rm = FALSE)
#>   S1 S2
#> A NA  9
#> B  3  6
aggregate_by_matrix(x, adj, colSumsMat, na.rm = TRUE)
#>   S1 S2
#> A  2  9
#> B  3  6
```
