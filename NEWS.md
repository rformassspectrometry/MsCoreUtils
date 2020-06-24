# MsCoreUtils 1.1

## Changes in 1.1.4

- Nothing yet.

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
