# Format Retention Time

These vectorised functions convert retention times from a numeric in
seconds to/from a character as "mm:ss". `rt2character()` performs the
numeric to character conversion while `rt2numeric()` performs the
character to numeric conversion. `formatRt()` does one of the other
depending on the input type.

## Usage

``` r
rt2numeric(rt)

rt2character(rt)

formatRt(rt)
```

## Arguments

- rt:

  A vector of retention times of length \> 1. Either a
  [`numeric()`](https://rdrr.io/r/base/numeric.html) in seconds or a
  [`character()`](https://rdrr.io/r/base/character.html) as `"mm:ss"`
  depending on the function.

## Value

A reformatted retention time.

## Author

Laurent Gatto

## Examples

``` r

## rt2numeric

rt2numeric("25:24")
#> [1] 1524
rt2numeric(c("25:24", "25:25", "25:26"))
#> [1] 1524 1525 1526

## rt2character

rt2character(1524)
#> [1] "25:24"
rt2character(1)
#> [1] "0:01"
rt2character(1:10)
#>  [1] "0:01" "0:02" "0:03" "0:04" "0:05" "0:06" "0:07" "0:08" "0:09" "0:10"

## formatRt

formatRt(1524)
#> [1] "25:24"
formatRt(1)
#> [1] "0:01"
formatRt(1:10)
#>  [1] "0:01" "0:02" "0:03" "0:04" "0:05" "0:06" "0:07" "0:08" "0:09" "0:10"
formatRt("25:24")
#> [1] 1524
formatRt(c("25:24", "25:25", "25:26"))
#> [1] 1524 1525 1526
```
