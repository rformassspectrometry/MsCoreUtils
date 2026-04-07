# Retry expression on failure

`retry()` retries, upon failure, the evaluation of an expression `exp`
for `ntimes` times waiting an increasing amount of time between tries,
i.e., waiting for `Sys.sleep(i * sleep_mult)` seconds between each try
`i`. If `expr` fails for `ntimes` times an error will be thrown.

## Usage

``` r
retry(expr, ntimes = 5L, sleep_mult = 0L)
```

## Arguments

- expr:

  Expression to be evaluated.

- ntimes:

  `integer(1)` with the number of times to try.

- sleep_mult:

  `numeric(1)` multiplier to define the increasing waiting time (in
  seconds).

## Note

Warnings are suppressed.

## Author

Johannes Rainer
