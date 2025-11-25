# Sequence with increasing difference between elements

`breaks_ppm` creates a sequence of numbers with increasing differences
between them. Parameter `ppm` defines the amount by which the difference
between values increases. The value for an element `i+1` is calculated
by adding `size` to the value of element `i` and in addition also the
`ppm(a, ppm)`, where `a` is the value of the element `i` plus `size`.
This iterative calculation is stopped once the value of an element is
larger than `to`. The last value in the result vector will thus not be
equal to `to` (which is in contrast to the base
[`seq()`](https://rdrr.io/r/base/seq.html) function) but slightly
higher.

A typical use case of this function would be to calculate breaks for the
binning of m/z values of mass spectra. This function allows to create
m/z-relative bin sizes which better represents measurement errors
observed on certain mass spectrometry instruments.

## Usage

``` r
breaks_ppm(from = 1, to = 1, by = 1, ppm = 0)
```

## Arguments

- from:

  `numeric(1)` with the value from which the sequence should start.

- to:

  `numeric(1)` defining the upper bound of the sequence. Note that the
  last value of the result will not be equal to `to` but equal to the
  first number in the sequence which is larger than this value.

- by:

  `numeric(1)` defining the constant part of the difference by which
  numbers should increase.

- ppm:

  `numeric(1)` defining the variable part of the difference by which
  numbers should increase (expressed in parts-per-million of the
  values).

## Value

`numeric` with the sequence of values with increasing differences. The
returned values include `from` and `to`.

## Author

Johannes Rainer

## Examples

``` r

res <- breaks_ppm(20, 50, by = 1, ppm = 50)
res
#>  [1] 20.00000 21.00105 22.00215 23.00330 24.00450 25.00575 26.00705 27.00840
#>  [9] 28.00980 29.01125 30.01275 31.01430 32.01590 33.01755 34.01926 35.02101
#> [17] 36.02281 37.02466 38.02656 39.02851 40.03051 41.03256 42.03467 43.03682
#> [25] 44.03902 45.04127 46.04357 47.04593 48.04833 49.05078 50.05328

## difference between the values increases (by ppm)
diff(res)
#>  [1] 1.001050 1.001100 1.001150 1.001200 1.001250 1.001300 1.001350 1.001400
#>  [9] 1.001450 1.001501 1.001551 1.001601 1.001651 1.001701 1.001751 1.001801
#> [17] 1.001851 1.001901 1.001951 1.002001 1.002052 1.002102 1.002152 1.002202
#> [25] 1.002252 1.002302 1.002352 1.002402 1.002452 1.002503
```
