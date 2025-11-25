# Spectra Distance/Similarity Measurements

These functions provide different normalized similariy/distance
measurements.

## Usage

``` r
ndotproduct(x, y, m = 0L, n = 0.5, na.rm = TRUE, ...)

dotproduct(x, y, m = 0L, n = 0.5, na.rm = TRUE, ...)

neuclidean(x, y, m = 0L, n = 0.5, na.rm = TRUE, ...)

navdist(x, y, m = 0L, n = 0.5, na.rm = TRUE, ...)

nspectraangle(x, y, m = 0L, n = 0.5, na.rm = TRUE, ...)
```

## Arguments

- x:

  `matrix`, two-columns e.g. m/z, intensity

- y:

  `matrix`, two-columns e.g. m/z, intensity

- m:

  `numeric`, weighting for the first column of `x` and `y` (e.g. "mz"),
  default: `0` means don't weight by the first column. For more details
  see the `ndotproduct` details section.

- n:

  `numeric`, weighting for the second column of `x` and `y` (e.g.
  "intensity"), default: `0.5` means effectly using `sqrt(x[,2])` and
  `sqrt(y[,2])`. For more details see the `ndotproduct` details section.

- na.rm:

  `logical(1)`, should `NA` be removed prior to calculation (default
  `TRUE`).

- ...:

  ignored.

## Value

`double(1)` value between `0:1`, where `0` is completely different and
`1` identically.

## Details

All functions that calculate normalized similarity/distance measurements
are prefixed with a *n*.

`ndotproduct`: the normalized dot product is described in Stein and
Scott 1994 as: \\NDP = \frac{\sum(W_1 W_2)^2}{\sum(W_1)^2
\sum(W_2)^2}\\; where \\W_i = x^m \* y^n\\, where \\x\\ and \\y\\ are
the m/z and intensity values, respectively. Please note also that \\NDP
= NCos^2\\; where NCos is the cosine value (i.e. the orthodox normalized
dot product) of the intensity vectors as described in Yilmaz et al.
2017. Stein and Scott 1994 empirically determined the optimal exponents
as `m = 3` and `n = 0.6` by analyzing ca. 12000 EI-MS data of 8000
organic compounds in the NIST Mass Spectral Library. MassBank (Horai et
al. 2010) uses `m = 2` and `n = 0.5` for small compounds. In general
with increasing values for `m`, high m/z values will be taken more into
account for similarity calculation. Especially when working with small
molecules, a value `m > 0` can be set to give a weight on the m/z values
to accommodate that shared fragments with higher m/z are less likely and
will mean that molecules might be more similar. Increasing `n` will
result in a higher importance of the intensity values. Most commonly
`m = 0` and `n = 0.5` are used.

`neuclidean`: the normalized euclidean distance is described in Stein
and Scott 1994 as: \\NED = (1 + \frac{\sum((W_1 -
W_2)^2)}{sum((W_2)^2)})^{-1}\\; where \\W_i = x^m \* y^n\\, where \\x\\
and \\y\\ are the m/z and intensity values, respectively. See the
details section about `ndotproduct` for an explanation how to set `m`
and `n`.

`navdist`: the normalized absolute values distance is described in Stein
and Scott 1994 as: \\NED = (1 + \frac{\sum(\|W_1 -
W_2\|)}{sum((W_2))})^{-1}\\; where \\W_i = x^m \* y^n\\, where \\x\\ and
\\y\\ are the m/z and intensity values, respectively. See the details
section about `ndotproduct` for an explanation how to set `m` and `n`.

`nspectraangle`: the normalized spectra angle is described in Toprak et
al 2014 as: \\NSA = 1 - \frac{2\*\cos^{-1}(W_1 \cdot W_2)}{\pi}\\; where
\\W_i = x^m \* y^n\\, where \\x\\ and \\y\\ are the m/z and intensity
values, respectively. The weighting was not originally proposed by
Toprak et al. 2014. See the details section about `ndotproduct` for an
explanation how to set `m` and `n`.

## Note

These methods are implemented as described in Stein and Scott 1994
(`navdist`, `ndotproduct`, `neuclidean`) and Toprak et al. 2014
(`nspectraangle`) but because there is no reference implementation
available we are unable to guarantee that the results are identical.
Note that the Stein and Scott 1994 normalized dot product method (and by
extension `ndotproduct`) corresponds to the square of the orthodox
normalized dot product (or cosine distance) used also commonly as
spectrum similarity measure (Yilmaz et al. 2017). Please see also the
corresponding discussion at the github pull request linked below. If you
find any problems or reference implementation please open an issue at
<https://github.com/rformassspectrometry/MsCoreUtils/issues>.

## References

Stein, S. E., and Scott, D. R. (1994). Optimization and testing of mass
spectral library search algorithms for compound identification. Journal
of the American Society for Mass Spectrometry, 5(9), 859–866.
[doi:10.1016/1044-0305(94)87009-8](https://doi.org/10.1016/1044-0305%2894%2987009-8)
.

Yilmaz, S., Vandermarliere, E., and Lennart Martens (2017). Methods to
Calculate Spectrum Similarity. In S. Keerthikumar and S. Mathivanan
(eds.), Proteome Bioinformatics: Methods in Molecular Biology, vol. 1549
(pp. 81).
[doi:10.1007/978-1-4939-6740-7_7](https://doi.org/10.1007/978-1-4939-6740-7_7)
.

Horai et al. (2010). MassBank: a public repository for sharing mass
spectral data for life sciences. Journal of mass spectrometry, 45(7),
703–714. [doi:10.1002/jms.1777](https://doi.org/10.1002/jms.1777) .

Toprak et al. (2014). Conserved peptide fragmentation as a benchmarking
tool for mass spectrometers and a discriminating feature for targeted
proteomics. Molecular & Cellular Proteomics : MCP, 13(8), 2056–2071.
[doi:10.1074/mcp.O113.036475](https://doi.org/10.1074/mcp.O113.036475) .

Pull Request for these distance/similarity measurements:
<https://github.com/rformassspectrometry/MsCoreUtils/pull/33>

## See also

Other distance/similarity functions:
[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)

## Author

`navdist`, `neuclidean`, `nspectraangle`: Sebastian Gibb

`ndotproduct`: Sebastian Gibb and Thomas Naake,
<thomasnaake@googlemail.com>

## Examples

``` r

x <- matrix(c(1:5, 1:5), ncol = 2, dimnames = list(c(), c("mz", "intensity")))
y <- matrix(c(1:5, 5:1), ncol = 2, dimnames = list(c(), c("mz", "intensity")))

ndotproduct(x, y)
#> [1] 0.7660906
ndotproduct(x, y, m = 2, n = 0.5)
#> [1] 0.9074293
ndotproduct(x, y, m = 3, n = 0.6)
#> [1] 0.9127553

neuclidean(x, y)
#> [1] 0.8003406

navdist(x, y)
#> [1] 0.6970151

nspectraangle(x, y)
#> [1] 0.5556013
```
