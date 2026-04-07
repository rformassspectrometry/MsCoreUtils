# Retry expression on failure

`retry()` retries, upon failure, the evaluation of an expression `exp`
for `ntimes` times waiting an increasing amount of time between tries,
i.e., waiting for `Sys.sleep(i * sleep_mult)` seconds between each try
`i`. If `expr` fails for `ntimes` times an error will be thrown.

## Usage

``` r
retry(expr, ntimes = 5L, sleep_mult = 0L, immediate_failure = "not found")
```

## Arguments

- expr:

  Expression to be evaluated.

- ntimes:

  `integer(1)` with the number of times to try.

- sleep_mult:

  `numeric(1)` multiplier to define the increasing waiting time (in
  seconds).

- immediate_failure:

  `character(1)` with a pattern that, if found in the error message
  eventually thrown by evaluating `expr`, would cause an immediate
  failure without retrying `expr`. This parameter is passed along to
  [`grepl()`](https://rdrr.io/r/base/grep.html) hence any regular
  expression is supported.

## Note

Warnings are suppressed.

## Author

Johannes Rainer
