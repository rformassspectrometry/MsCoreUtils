# Spectral entropy

These functions allow to calculate entropy measurements of an MS/MS
spectrum based on the metrics suggested by Li et al.
(https://doi.org/10.1038/s41592-021-01331-z). Spectral entropy and
normalized entropy are used to measure the complexity of an spectra.
MassBank of North America (MoNA) defines spectra entropy as the
intensity weighted spectral peak number
(https://mona.fiehnlab.ucdavis.edu/documentation/entropy). Additionally
it is suggested to consider spectra with a normalized entropy larger
than 0.8, or a spectral entropy larger than 3 as low-quality spectra.

## Usage

``` r
entropy(x)

nentropy(x)
```

## Arguments

- x:

  `numeric`, intensities of the fragment ions.

## Value

`numeric`: (normalized) entropy of `x`.

## Author

Mar Garcia-Aloy

## Examples

``` r
spectrum <- rbind(c(41.04, 37.16), c(69.07, 66.83), c(86.1, 999.0))

entropy(spectrum[,2])
#> [1] 0.3737888
nentropy(spectrum[,2])
#> [1] 0.3402372
```
