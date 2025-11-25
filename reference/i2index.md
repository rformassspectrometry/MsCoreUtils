# Input parameter check for subsetting operations

`i2index` is a simple helper function to be used in subsetting
functions. It checks and converts the parameter `i`, which can be of
type `integer`, `logical` or `character` to integer vector that can be
used as index for subsetting.

## Usage

``` r
i2index(i, length = length(i), names = NULL)
```

## Arguments

- i:

  `character` `logical` or `integer` used in `[i]` for subsetting.

- length:

  `integer` representing the `length` of the object to be subsetted.

- names:

  `character` with the names (rownames or similar) of the object. This
  is only required if `i` is of type `character`.

## Value

`integer` with the indices

## Author

Johannes Rainer

## Examples

``` r

## With `i` being an `integer`
i2index(c(4, 1, 3), length = 10)
#> [1] 4 1 3

## With `i` being a `logical`
i2index(c(TRUE, FALSE, FALSE, TRUE, FALSE), length = 5)
#> [1] 1 4

## With `i` being a `character`
i2index(c("b", "a", "d"), length = 5, names = c("a", "b", "c", "d", "e"))
#> [1] 2 1 4
```
