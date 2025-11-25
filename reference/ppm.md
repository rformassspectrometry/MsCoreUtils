# PPM - Parts per Million

`ppm` is a small helper function to determine the parts-per-million for
a user-provided value and ppm.

## Usage

``` r
ppm(x, ppm)
```

## Arguments

- x:

  `numeric`, value(s) used for ppm calculation, e.g. mz value(s).

- ppm:

  `numeric`, parts-per-million (ppm) value(s).

## Value

`numeric`: parts-per-million of `x` (always a positive value).

## Author

Sebastian Gibb

## Examples

``` r
ppm(c(1000, 2000), 5)
#> [1] 0.005 0.010

ppm(c(-300, 200), 5)
#> [1] 0.0015 0.0010
```
