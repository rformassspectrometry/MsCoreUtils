# Optimized GNPS Modified Cosine Similarity via Chain-DP

Computes the GNPS (Global Natural Products Social molecular networking)
modified cosine similarity score between two mass spectra using a fused
join + score algorithm based on Chain-DP (Chain Dynamic Programming).
This function combines peak matching and scoring in a single C call,
achieving consequent speedup over the standard
[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)
implementation while maintaining exact mathematical equivalence
(differences ≤ 2.2e-16).

**Algorithm**: Chain-DP optimal assignment. When spectra are sanitized,
the bipartite matching graph forms simple chains (not arbitrary
networks). This enables O(n+m) greedy scoring for most of the pairs,
with exact Hungarian solver O(k³) only for rare conflicts (k ≈ 3–5).

**Complexity**: O(n+m) time, O(n+m) memory (vs. O(n³) time, O(n²) memory
for full Hungarian).

## Usage

``` r
gnps_chain_dp(
  x,
  y,
  xPrecursorMz = NA_real_,
  yPrecursorMz = NA_real_,
  tolerance = 0,
  ppm = 0,
  ...,
  matchedPeaksCount = FALSE
)
```

## Arguments

- x:

  Numeric `matrix` with query spectrum peaks (2 columns: mz, intensity).
  Must be sorted by mz in ascending order.

- y:

  Numeric `matrix` with library spectrum peaks (2 columns: mz,
  intensity). Must be sorted by mz in ascending order.

- xPrecursorMz:

  `numeric(1)`, precursor m/z for query spectrum.

- yPrecursorMz:

  `numeric(1)`, precursor m/z for library spectrum.

- tolerance:

  `numeric(1)`, absolute tolerance in Daltons.

- ppm:

  `numeric(1)`, relative tolerance in ppm.

- ...:

  ignored.

- matchedPeaksCount:

  `logical(1)`; if `TRUE`, return both score and matched-peak count,
  otherwise return score only.

## Value

A `numeric` vector of length 1 by default (score), or length 2 when
`matchedPeaksCount = TRUE` (`c(score, matched_peaks)`).

## Details

The modified cosine score is computed as:

\$\$ \text{score}(i,j) = \frac{\sqrt{I_x(i)}}{\sqrt{\sum I_x}} \times
\frac{\sqrt{I_y(j)}}{\sqrt{\sum I_y}} \$\$

where the sum is over unique m/z values (first occurrence of
duplicates).

The total score is the sum of all optimally assigned peak pairs, found
via:

1.  **Direct matching**: `join(x, y, type="outer")` - closest one-to-one

2.  **Shifted matching**: `join(x + pdiff, y, type="outer")` where
    `pdiff = yPrecursorMz - xPrecursorMz`

3.  **Optimal assignment via Chain-DP**: For each query peak, pick the
    better of its direct and shifted matches. When spectra are
    sanitized, conflicts are rare (~1%) and resolved optimally with
    exact Hungarian.

**Precursor threshold**: Shifted matching is skipped when
`|pdiff| ≤ tolerance + ppm × max(xPrecursorMz, yPrecursorMz) × 1e-6`,
i.e., when the precursor difference is within the peak matching
tolerance. This is scientifically correct (no meaningful neutral loss
when pdiff ≈ tolerance) but differs from the existing
[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)
implementation, which only skips when `pdiff == 0.0` exactly. See
References for details.

## Prerequisites

**CRITICAL**: Input spectra MUST be sanitized before calling this
function:

- **Unique m/z values**: no two peaks in the same spectrum should have
  m/z values close enough to match each other (i.e.,
  `|mz_i - mz_j| > tolerance` for all peak pairs i,j within the same
  spectrum)

- **Non-negative intensities** (no NaN/NA/Inf)

- **Sorted by m/z** in ascending order

The chain-DP algorithm assumes at most one direct match and one shifted
match per peak — a property that holds when peaks are well-separated.
Unsanitized spectra will produce incorrect scores silently.

**How to sanitize**:


    library(Spectra)
    sps <- reduceSpectra(sps)   # Remove peaks closer than tolerance
    sps <- combinePeaks(sps)    # Merge duplicate m/z
    sps <- scalePeaks(sps)      # Normalize intensities

## References

Wang M, Carver JJ, Phelan VV, et al. (2016). "Sharing and community
curation of mass spectrometry data with Global Natural Products Social
Molecular Networking." *Nature Biotechnology* 34:828–837.
[doi:10.1038/nbt.3597](https://doi.org/10.1038/nbt.3597)

Dührkop K, Fleischauer M, Ludwig M, et al. (2019). "SIRIUS 4: a rapid
tool for turning tandem mass spectra into metabolite structure
information." *Nature Methods* 16:299–302.
[doi:10.1038/s41592-019-0344-8](https://doi.org/10.1038/s41592-019-0344-8)

Chain-DP algorithm implementation:
<https://github.com/sirius-ms/sirius/blob/stable/spectral_alignment/src/main/java/de/unijena/bionf/fastcosine/FastCosine.java>

## See also

[`gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)
for the standard (backward-compatible) implementation.

[`join_gnps()`](https://rformassspectrometry.github.io/MsCoreUtils/reference/gnps.md)
for peak matching only.

## Author

Adriano Rutz

## Examples

``` r
# Example spectra (sanitized: sorted, unique m/z, no NAs)
x <- cbind(mz = c(10, 36, 63, 91, 93), intensity = c(14, 15, 999, 650, 1))
y <- cbind(mz = c(10, 12, 50, 63, 105), intensity = c(35, 5, 16, 999, 450))

# Compute modified cosine via chain-DP (hot path)
result <- gnps_chain_dp(x, y,
                        xPrecursorMz = 91.0,
                        yPrecursorMz = 105.0,
                        tolerance = 0.01,
                        ppm = 10,
                        matchedPeaksCount=TRUE)
result[1L]
#> [1] 0.9923501
result[2L]
#> [1] 4

# Compare with standard implementation (should agree within 1e-15)
matches <- join_gnps(x[,1], y[,1], 91.0, 105.0, 0.01, 10)
score_std <- gnps(x[matches$x, ], y[matches$y, ])
abs(result[1L] - score_std) < 1e-10  # TRUE
#> [1] TRUE
```
