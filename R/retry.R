#' @title Retry expression on failure
#'
#' @description
#'
#' `retry()` retries, upon failure, the evaluation of an expression `exp` for
#' `ntimes` times waiting an increasing amount of time
#' between tries, i.e., waiting for `Sys.sleep(i * sleep_mult)` seconds between
#' each try `i`. If `expr` fails for `ntimes` times an error will be thrown.
#'
#' @param expr Expression to be evaluated.
#'
#' @param ntimes `integer(1)` with the number of times to try.
#'
#' @param sleep_mult `numeric(1)` multiplier to define the increasing waiting
#'     time (in seconds).
#'
#' @param retry_on `character(1)` pattern for the error message to retry `expr`.
#'     Defaults to `retry_on = "*"` hence retrying `expr` on any error that
#'     occurs. This allows to restrict retrying `expr` for specific cases,
#'     such as temporary internet connection problems. The pattern defined by
#'     `retry_on` is directly passed to the `grepl()` function.
#'
#' @param warningsAsErrors `logical(1)` whether warnings should be converted
#'     to errors. Defaults to `warningsAsErrors = FALSE`.
#'
#' @param verbose `logical(1)` whether additional messages on eventually caught
#'     errors should be printed. Defaults to `verbose = FALSE`.
#'
#' @param ... optional parameters passed to `grepl()`.
#'
#' @note
#'
#' Warnings are suppressed.
#'
#' @author Johannes Rainer
#'
#' @export
#'
#' @examples
#'
#' ## In the example below content is read from a web page that might
#' ## temporarily be offline. It is assumed that the error message contains
#' ## the pattern `"temporarily"` in which case the call is repeated 7 times
#' ## with an increasing interval between tries to allow the resource to
#' ## become available again.
#' \dontrun{
#'     res <- retry(readLines("https://some-unreliable-web-content"),
#'                  retry_on = "temporarily", ntimes = 7L, sleep_mult = 10)
#' }
retry <- function(expr, ntimes = 5L, sleep_mult = 0L,
                  retry_on = "*", warningsAsErrors = FALSE,
                  verbose = FALSE,...) {
    res <- NULL
    if (warningsAsErrors) {
        w <- getOption("warn")
        options(warn = 2)
        on.exit(options(warn = w))
    }
    for (i in seq_len(ntimes)) {
        ## the `eval.parent` comes from https://stackoverflow.com/questions/20596902/r-avoiding-restarting-interrupted-promise-evaluation-warning
        res <- tryCatch(eval.parent(substitute(expr)), error = function(e) e)
        if (inherits(res, "error")) {
            if (grepl(retry_on, res$message, ...) && i < ntimes) {
                if (verbose)
                    message("Caught error: ", res$message, ". Will retry in ",
                            i * sleep_mult, " seconds")
                Sys.sleep(i * sleep_mult)
            }
            else stop(res)
        } else break
    }
    res
}
