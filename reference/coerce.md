# Coerce functions

- `asInteger`: convert `x` to an `integer` and throw an error if `x` is
  not a `numeric`.

## Usage

``` r
asInteger(x)
```

## Arguments

- x:

  input argument.

## Author

Johannes Rainer

## Examples

``` r
## Convert numeric to integer
asInteger(3.4)
#> [1] 3

asInteger(3)
#> [1] 3
```
