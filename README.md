# Low level functions for MS data

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/RforMassSpectrometry/MsCoreUtils/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/RforMassSpectrometry/MsCoreUtils/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov](https://codecov.io/gh/rformassspectrometry/MsCoreUtils/branch/main/graph/badge.svg?token=sv2V5zQjib)](https://codecov.io/gh/rformassspectrometry/MsCoreUtils)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)
[![years in bioc](http://bioconductor.org/shields/years-in-bioc/MsCoreUtils.svg)](https://bioconductor.org/packages/release/bioc/html/MsCoreUtils.html)
[![Ranking by downloads](http://bioconductor.org/shields/downloads/release/MsCoreUtils.svg)](https://bioconductor.org/packages/stats/bioc/MsCoreUtils/)
[![build release](http://bioconductor.org/shields/build/release/bioc/MsCoreUtils.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/MsCoreUtils/)
[![build devel](http://bioconductor.org/shields/build/devel/bioc/MsCoreUtils.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/MsCoreUtils/)


`MsCoreUtils` defines low-level functions for mass spectrometry data and is
independent of any high-level data structures.
These functions include mass spectra processing functions
(noise estimation, smoothing, binning),
quantitative aggregation functions (median polish, robust summarisation, ...),
missing data imputation, data normalisation (quantiles, vsn, ...)
as well as misc helper functions, that are used across high-level
data structure within the
[R for Mass Spectrometry packages](https://www.rformassspectrometry.org/pkgs/).

See the package [homepage](https://rformassspectrometry.github.io/MsCoreUtils)
for more information.


# Installation

The package can be installed with

```r
install.packages("BiocManager")
BiocManager::install("MsCoreUtils")
```


# Contributions

Contributions are highly welcome and should follow the [contribution
guidelines](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html#contributions).
Also, please check the coding style guidelines in the [RforMassSpectrometry
vignette](https://rformassspectrometry.github.io/RforMassSpectrometry/articles/RforMassSpectrometry.html).


## Previous external contributions

- Sigurdur Smarason (@SiggiSmara): weighted moving average (https://github.com/sgibb/MALDIquant/pull/54)
- Thomas Naake (@tnaake): dotproduct calculation (https://github.com/rformassspectrometry/MsCoreUtils/pull/17)
- Adriaan Sticker: `robustSummary` aggregation function (originally contributed to `MSnbase`)
