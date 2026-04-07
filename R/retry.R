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
#'     occurrs. This allows to restrict retrying `expr` for specific cases,
#'     such as temporary internet connection problems. The pattern defined by
#'     `retry_on` is directly passed to the `grepl()` function.
#'
#' @param ... optional parameters passed to `grepl()`.
#'
#' @note
#'
#' Warnings are suppressed.
#'
#' @author Johannes Rainer
#'
#' @importFrom methods is
#'
#' @export
retry <- function(expr, ntimes = 5L, sleep_mult = 0L,
                  retry_on = "*", ...) {
    res <- NULL
    for (i in seq_len(ntimes)) {
        res <- suppressWarnings(tryCatch(expr, error = function(e) e))
        if (is(res, "simpleError")) {
            if (grepl(retry_on, res$message, ...) && i < ntimes)
                Sys.sleep(i * sleep_mult)
            else stop(res)
        } else break
    }
    res
}
