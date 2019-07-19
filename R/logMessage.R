#' Create Log Messages
#'
#' This function creates a standardised log message and append it to an
#' existing vector of log messages.
#'
#' @param x `character`, existing log messages.
#' @param ... one or more object to be converted to `character`, the new log
#' message.
#' @param date `logical(1)`, should the [`date()`] append to the message?
#'
#' @return `character` vector with new log message appended.
#'
#' @author Sebastian Gibb
#' @export
#' @examples
#' logMessage(NULL, "Important Stuff")
#' logMessage(NULL, "Important Stuff", date = FALSE)
#' logMessage("Old message",
#'            "New number: ", 3, " and letters ",
#'            paste0(letters[1:4], collapse=", "))
logMessage <- function(x, ..., date=TRUE) {
    l <- list(...)
    if (date)
        l <- c(l, " [", date(), "]")
    c(x, do.call(paste0, l))
}
