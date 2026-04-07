# Retry expression on failure

`retry()` retries, upon failure, the evaluation of an expression `exp`
for `ntimes` times waiting an increasing amount of time between tries,
i.e., waiting for `Sys.sleep(i * sleep_mult)` seconds between each try
`i`. If `expr` fails for `ntimes` times an error will be thrown.

## Usage

``` r
retry(expr, ntimes = 5L, sleep_mult = 0L, retry_on = "*", ...)
```

## Arguments

- expr:

  Expression to be evaluated.

- ntimes:

  `integer(1)` with the number of times to try.

- sleep_mult:

  `numeric(1)` multiplier to define the increasing waiting time (in
  seconds).

- retry_on:

  `character(1)` pattern for the error message to retry `expr`. Defaults
  to `retry_on = "*"` hence retrying `expr` on any error that occurrs.
  This allows to restrict retrying `expr` for specific cases, such as
  temporary internet connection problems. The pattern defined by
  `retry_on` is directly passed to the
  [`grepl()`](https://rdrr.io/r/base/grep.html) function.

- ...:

  optional parameters passed to
  [`grepl()`](https://rdrr.io/r/base/grep.html).

## Note

Warnings are suppressed.

## Author

Johannes Rainer
