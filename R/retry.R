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
#' @param immediate_failure `character(1)` with a pattern that, if found in
#'     the error message eventually thrown by evaluating `expr`, would cause an
#'     immediate failure without retrying `expr`. This parameter is passed
#'     along to `grepl()` hence any regular expression is supported.
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
                  immediate_failure = "not found") {
    res <- NULL
    for (i in seq_len(ntimes)) {
        res <- suppressWarnings(tryCatch(expr, error = function(e) e))
        if (is(res, "simpleError")) {
            if (i == ntimes || grepl(immediate_failure, res$message))
                stop(res)
            Sys.sleep(i * sleep_mult)
        } else break
    }
    res
}
