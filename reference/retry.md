# Retry expression on failure

`retry()` retries, upon failure, the evaluation of an expression `exp`
for `ntimes` times waiting an increasing amount of time between tries,
i.e., waiting for `Sys.sleep(i * sleep_mult)` seconds between each try
`i`. If `expr` fails for `ntimes` times an error will be thrown.

## Usage

``` r
retry(
  expr,
  ntimes = 5L,
  sleep_mult = 0L,
  retry_on = "*",
  warningsAsErrors = FALSE,
  verbose = FALSE,
  ...
)
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
  to `retry_on = "*"` hence retrying `expr` on any error that occurs.
  This allows to restrict retrying `expr` for specific cases, such as
  temporary internet connection problems. The pattern defined by
  `retry_on` is directly passed to the
  [`grepl()`](https://rdrr.io/r/base/grep.html) function.

- warningsAsErrors:

  `logical(1)` whether warnings should be converted to errors. Defaults
  to `warningsAsErrors = FALSE`.

- verbose:

  `logical(1)` whether additional messages on eventually caught errors
  should be printed. Defaults to `verbose = FALSE`.

- ...:

  optional parameters passed to
  [`grepl()`](https://rdrr.io/r/base/grep.html).

## Note

Warnings are suppressed.

## Author

Johannes Rainer, Gabriele Tomè

## Examples

``` r

## In the example below content is read from a web page that might
## temporarily be offline. It is assumed that the error message contains
## the pattern `"temporarily"` in which case the call is repeated 7 times
## with an increasing interval between tries to allow the resource to
## become available again.
if (FALSE) { # \dontrun{
    res <- retry(readLines("https://some-unreliable-web-content"),
                 retry_on = "temporarily", ntimes = 7L, sleep_mult = 10)
} # }
```
