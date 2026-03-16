# GNPS spectrum similarity scores

The `join_gnps()`, `join_gnps_r()`, `gnps()` and `gnps_r()` functions
allow to calculate spectra similarity scores as used in
[GNPS](https://gnps.ucsd.edu/). The `_r` versions are the reference
implementation in R with full support of all parameters, while
`join_gnps()` and `gnps()` are implemented in C and therefore faster.
The geneal approach of the similarity calculation matches first peaks
between the two spectra directly using a user-defined `ppm` and/or
`tolerance` as well as using a fixed delta m/z (considering the same
`ppm` and `tolerance`) that is defined by the difference of the compared
spectras' precursor m/z values. For peaks that match multiple peaks in
the other spectrum only the matching peak pair with the higher
value/similarity is considered in the final similarity score
calculation. Note that GNPS similarity scores are calculated only if
**both** functions are used together.

- `join_gnps_r()`, `join_gnps()`: matches/maps peaks between spectra
  with the same approach as in GNPS: peaks are considered matching if a)
  the difference in their m/z values is smaller than defined by
  `tolerance` and `ppm` (this is the same as `joinPeaks()`) **and** b)
  the difference of their m/z *adjusted* for the difference of the
  spectras' precursor is smaller than defined by `tolerance` and `ppm`.
  Based on this definition, peaks in `x` can match up to two peaks in
  `y` hence returned peak indices might be duplicated. Note that if one
  of `xPrecursorMz` or `yPrecursorMz` are `NA` or if both are the same,
  the results are the same as with
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md).
  The function returns a `list` of two `integer` vectors with the
  indices of the peaks matching peaks in the other spectrum or `NA`
  otherwise. The `join_gnps()` function is implemented in C and uses an
  *outer* join of the peaks (i.e., `type = "outer"`).

- `gnps_r()`, gnps(): calculates the GNPS similarity score on peak
  matrices' previously *aligned* (matched) with `join_gnps()`. For
  multi-mapping peaks the pair with the higher similarity are considered
  in the final score calculation. By setting `matchedPeaksCount = TRUE`
  the number of peak pairs on which the score was calculated is returned
  in addition to the similarity score. By default (with
  `matchedPeaksCount = FALSE`) a `numeric(1)` with the similarity score
  is returned. For `matchedPeaksCount = TRUE` a `numeric(2)` is returned
  with the first element being the similarity scoreand the second the
  number of matched peak pairs. The `gnps()` function is implemented in
  C while the `gnps_r()` function is based on the implementation from
  the references below.

## Usage

``` r
gnps_r(x, y, ..., matchedPeaksCount = FALSE)

join_gnps_r(
  x,
  y,
  xPrecursorMz = NA_real_,
  yPrecursorMz = NA_real_,
  tolerance = 0,
  ppm = 0,
  type = "outer",
  ...
)

join_gnps(
  x,
  y,
  xPrecursorMz = NA_real_,
  yPrecursorMz = NA_real_,
  tolerance = 0,
  ppm = 0,
  type = "outer",
  ...
)

gnps(x, y, ..., matchedPeaksCount = FALSE)
```

## Arguments

- x:

  for `join_gnps()` and `join_gnps_r()`: `numeric` with m/z values from
  a spectrum. For `gnps()` and `gnps_r()`: `matrix` with two columns
  `"mz"` and `"intensity"` containing the peaks **aligned** with peaks
  in `y` (with `join_gnps()` or `join_gnps_r()`).

- y:

  for `join_gnps()` and `join_gnps_r()`: `numeric` with m/z values from
  a spectrum. For `gnps()` and `gnps_r()`: `matrix` with two columns
  `"mz"` and `"intensity"` containing the peaks **aligned** with peaks
  in `x` (with `join_gnps()` or `join_gnps_r()`).

- ...:

  for `join_gnps()` and `join_gnps_r()`: optional parameters passed to
  the
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)
  function. For `gnps()` and `gnps_r()`: ignored.

- matchedPeaksCount:

  `logical(1)` whether the number of peak pairs on which the score was
  calculated should be returned. Defaults to
  `matchedPeaksCount = FALSE`. If set to `matchedPeaksCount = TRUE` a
  `numeric` of length 2 is returned.

- xPrecursorMz:

  for `join_gnps()` and `join_gnps_r()`: `numeric(1)` with the precursor
  m/z of the spectrum `x`.

- yPrecursorMz:

  for `join_gnps()` or `join_gnps_r()`: `numeric(1)` with the precursor
  m/z of the spectrum `y`.

- tolerance:

  for `join_gnps()` and `join_gnps_r()`: `numeric(1)` defining a
  constant maximal accepted difference between m/z values of peaks from
  the two spectra to be matched/mapped.

- ppm:

  for `join_gnps()` and `join_gnps_r()`: `numeric(1)` defining a
  relative, m/z-dependent, maximal accepted difference between m/z
  values of peaks from the two spectra to be matched/mapped.

- type:

  for `join_gnps_r()`: `character(1)` specifying the type of join that
  should be performed. See
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)
  for details and options. Defaults to `type = "outer"`.

## Value

See function definition in the description section.

## Details

The implementation of `gnps_r()` bases on the R code from the
publication listed in the references.

## References

Xing S, Hu Y, Yin Z, Liu M, Tang X, Fang M, Huan T. Retrieving and
Utilizing Hypothetical Neutral Losses from Tandem Mass Spectra for
Spectral Similarity Analysis and Unknown Metabolite Annotation. *Anal
Chem.* 2020 Nov 3;92(21):14476-14483.
[doi:10.1021/acs.analchem.0c02521](https://doi.org/10.1021/acs.analchem.0c02521)
.

## See also

[`gnps_chain_dp()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps_chain_dp.md)
for am optimized and fast implementation based on the Chain-DP
algorithm.

Other grouping/matching functions:
[`bin()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/binning.md),
[`closest()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)

Other distance/similarity functions:
[`distance`](https://rformassspectrometry.github.io/MsCoreUtils/reference/distance.md)

## Author

Johannes Rainer, Michael Witting, based on the code from Xing *et al.*
(2020). Adriano Rutz for the C implementations.

## Examples

``` r

## Define spectra
x <- cbind(mz = c(10, 36, 63, 91, 93), intensity = c(14, 15, 999, 650, 1))
y <- cbind(mz = c(10, 12, 50, 63, 105), intensity = c(35, 5, 16, 999, 450))
## The precursor m/z
pmz_x <- 91
pmz_y <- 105

## Plain join identifies only 2 matching peaks
join(x[, 1], y[, 1])
#> $x
#> [1]  1 NA  2 NA  3  4  5 NA
#> 
#> $y
#> [1]  1  2 NA  3  4 NA NA  5
#> 

## join_gnps_r finds 4 matches
join_gnps_r(x[, 1], y[, 1], pmz_x, pmz_y)
#> $x
#>  [1]  1  2  2  3  4  4  5 NA NA NA
#> 
#> $y
#>  [1]  1 NA  3  4 NA  5 NA  2  3  5
#> 

## with one of the two precursor m/z being NA, the result are the same as
## with join.
join_gnps_r(x[, 1], y[, 1], pmz_x, yPrecursorMz = NA)
#> $x
#> [1]  1 NA  2 NA  3  4  5 NA
#> 
#> $y
#> [1]  1  2 NA  3  4 NA NA  5
#> 

## Calculate GNPS similarity score:
map <- join_gnps_r(x[, 1], y[, 1], pmz_x, pmz_y)
gnps_r(x[map[[1]], ], y[map[[2]], ])
#> [1] 0.9923501
```
