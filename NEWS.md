# MsCoreUtils 1.7

## MsCoreUtils 1.7.0

# MsCoreUtils 1.5

## Changes in 1.5.1

- Add `which.first` and `which.last`.

## Changes in 1.5.0

- New Bioc devel version

# MsCoreUtils 1.3

## Changes in 1.3.3

- Add `join_gnps` and `gnps` to allow calculation of GNPS spectra similarity
  scores.

## Changes in 1.3.2

- Add `rt2numeric()`, `rt2character()` and `formatRt()`.
- New `impute_fun()` function for user-provide imputation function.

## Changes in 1.3.1

- Add Josep Badia Aparicio as a contributor

## Changes in 1.3.0

- New Bioc devel version

# MsCoreUtils 1.1

## Changes in 1.1.7

- Rewrite `c("left", "right", "inner", "outer")` `join` in C <2020-10-06 Tue>.

## Changes in 1.1.6

- Rewrite `closest` in C <2020-09-24 Thu>.
- Fix [#65](https://github.com/rformassspectrometry/MsCoreUtils/issues/65) and
  [#66](https://github.com/rformassspectrometry/MsCoreUtils/issues/66).

## Changes in 1.1.5

- Add `...` to functions to join and compare peaks; see also
  [#131](https://github.com/rformassspectrometry/Spectra/issues/131).

## Changes in 1.1.4

- Change references to `Feature` to `QFeatures` <2020-07-14 Tue>
- Ensure `closest` accept just argument `tolerance` of length 1 or `length(x)`;
  see also [#61](https://github.com/rformassspectrometry/MsCoreUtils/issue/61),
  [PR #62](https://github.com/rformassspectrometry/MsCoreUtils/pull/62)
  <2020-08-07 Thu>.
- The `tolerance` argument in `closest` should now be of length 1 or of
  `length(x)` (was `length(table)` before) <2020-08-20 Thu>.

## Changes in 1.1.3

- For an empty `table` `closest` and `common` return a vector of length `x`
  with `NA` or `FALSE`, respectively (instead of `1` and `TRUE`).
  Fixes [#55](https://github.com/rformassspectrometry/MsCoreUtils/pull/55)
  <2020-06-18 Thu>.
- `closest` and `common` ignore `NA` in `table` <2020-06-19 Fri>.
- Fix `rbindFill` for single `data.frame` or `DataFrame` as input <2020-06-23 Tue>.

## Changes in 1.1.2

- New `colCounts()` aggregation function <2020-05-27 Wed>.

## Changes in 1.1.1

- Add some popular distance/similarity metrices:
  `ndotproduct` `neuclidean` `navdist` `nspectraangle`; see also
  [PR #33](https://github.com/rformassspectrometry/MsCoreUtils/pull/33).

- Add deprecation note to `dotproduct` <2020-05-22 Fri>.

## Changes in 1.1.0

- Bioconductor devel version (Bioc 3.12)

# MsCoreUtils 1.0

## Changes in 1.0.0.

- Bioconductor release version (Bioc 3.11)

# MsCoreUtils 0.99

## MsCoreUtils 0.99.3

- Trigger build.

## MsCoreUtils 0.99.2

- Provide more comprehensive description.
- Add vignette.

## MsCoreUtils 0.99.1

- Additional functions, and using Author@R to specify (unique)
  RforMassSpectrometry Package Maintainer.

## MsCoreUtils 0.99.0

- First release of `MsCoreUtils` with core function to get extreme
  values, grouping/matching, noise/smoothing, similarity measurements,
  various helper function, and function to process (impute and
  normalise) quantitative features.
