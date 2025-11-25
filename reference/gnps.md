# GNPS spectrum similarity scores

The `join_gnps` and `gnps` functions allow to calculate spectra
similarity scores as used in [GNPS](https://gnps.ucsd.edu/). The
approach matches first peaks between the two spectra directly using a
user-defined ppm and/or tolerance as well as using a fixed delta m/z
(considering the same ppm and tolerance) that is defined by the
difference of the two spectras' precursor m/z values. For peaks that
match multiple peaks in the other spectrum only the matching peak pair
with the higher value/similarity is considered in the final similarity
score calculation. Note that GNPS similarity scores are calculated only
if the two functions are used together.

- `join_gnps`: matches/maps peaks between spectra with the same approach
  as in GNPS: peaks are considered matching if a) the difference in
  their m/z values is smaller than defined by `tolerance` and `ppm`
  (this is the same as `joinPeaks`) **and** b) the difference of their
  m/z *adjusted* for the difference of the spectras' precursor is
  smaller than defined by `tolerance` and `ppm`. Based on this
  definition, peaks in `x` can match up to two peaks in `y` hence
  returned peak indices might be duplicated. Note that if one of
  `xPrecursorMz` or `yPrecursorMz` are `NA` or if both are the same, the
  results are the same as with
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md).
  The function returns a `list` of two `integer` vectors with the
  indices of the peaks matching peaks in the other spectrum or `NA`
  otherwise.

- `gnps`: calculates the GNPS similarity score on peak matrices'
  previously *aligned* (matched) with `join_gnps`. For multi-mapping
  peaks the pair with the higher similarity are considered in the final
  score calculation.

## Usage

``` r
gnps(x, y, ...)

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
```

## Arguments

- x:

  for `join_gnps`: `numeric` with m/z values from a spectrum. For
  `gnps`: `matrix` with two columns `"mz"` and `"intensity"` containing
  the peaks **aligned** with peaks in `y` (with `join_gnps`).

- y:

  for `join_gnps`: `numeric` with m/z values from a spectrum. For
  `gnps`: `matrix` with two columns `"mz"` and `"intensity"` containing
  the peaks **aligned** with peaks in `x` (with `join_gnps`).

- ...:

  for `join_gnps`: optional parameters passed to the
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)
  function. For `gnps`: ignored.

- xPrecursorMz:

  for `join_gnps`: `numeric(1)` with the precursor m/z of the spectrum
  `x`.

- yPrecursorMz:

  for `join_gnps`: `numeric(1)` with the precursor m/z of the spectrum
  `y`.

- tolerance:

  for `join_gnps`: `numeric(1)` defining a constant maximal accepted
  difference between m/z values of peaks from the two spectra to be
  matched/mapped.

- ppm:

  for `join_gnps`: `numeric(1)` defining a relative, m/z-dependent,
  maximal accepted difference between m/z values of peaks from the two
  spectra to be matched/mapped.

- type:

  for `join_gnps`: `character(1)` specifying the type of join that
  should be performed. See
  [`join()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)
  for details and options. Defaults to `type = "outer"`.

## Value

See function definition in the description section.

## Details

The implementation of `gnps` bases on the R code from the publication
listed in the references.

## References

Xing S, Hu Y, Yin Z, Liu M, Tang X, Fang M, Huan T. Retrieving and
Utilizing Hypothetical Neutral Losses from Tandem Mass Spectra for
Spectral Similarity Analysis and Unknown Metabolite Annotation. *Anal
Chem.* 2020 Nov 3;92(21):14476-14483.
[doi:10.1021/acs.analchem.0c02521](https://doi.org/10.1021/acs.analchem.0c02521)
.

## See also

Other grouping/matching functions:
[`bin()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/binning.md),
[`closest()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/matching.md)

Other distance/similarity functions:
[`distance`](https://rformassspectrometry.github.io/MsCoreUtils/reference/distance.md)

## Author

Johannes Rainer, Michael Witting, based on the code from Xing *et al.*
(2020).

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

## join_gnps finds 4 matches
join_gnps(x[, 1], y[, 1], pmz_x, pmz_y)
#> $x
#>  [1]  1  2  2  3  4  4  5 NA NA NA
#> 
#> $y
#>  [1]  1 NA  3  4 NA  5 NA  2  3  5
#> 

## with one of the two precursor m/z being NA, the result are the same as
## with join.
join_gnps(x[, 1], y[, 1], pmz_x, yPrecursorMz = NA)
#> $x
#> [1]  1 NA  2 NA  3  4  5 NA
#> 
#> $y
#> [1]  1  2 NA  3  4 NA NA  5
#> 

## Calculate GNPS similarity score:
map <- join_gnps(x[, 1], y[, 1], pmz_x, pmz_y)
gnps(x[map[[1]], ], y[map[[2]], ])
#> [1] 0.9923501
```
