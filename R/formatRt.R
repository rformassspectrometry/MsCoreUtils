##' This vectorised function converts retention times from a numeric
##' in seconds to/from a character as "mm:ss".
##'
##' @title Format Retention Time
##'
##' @param rt A vector of retention times of length > 1. Either a
##'     `numeric()` in seconds or a `character()` as `"mm:ss"`.
##' 
##' @return A reformatted retention time.
##' 
##' @author Laurent Gatto
##'
##' @export
##'
##' @examples
##' formatRt(1524)
##' formatRt(1)
##' formatRt(1:10)
##' formatRt("25:24")
##' formatRt(c("25:24", "25:25", "25:26"))
formatRt <- function(rt) {
    stopifnot(length(rt) > 0)
    ans <- NA
    if (is.numeric(rt)) {
        min <- floor(rt / 60)
        sec <- round(rt - (min * 60))
        ans <- sprintf("%d:%02d", min, sec)
    } else if (is.character(rt)) {
        ans <- strsplit(rt, ":")
        ans <- sapply(ans, function(x) {
            x <- as.numeric(x)
            60 * x[1] + x[2]
        })
    } else {
        warning("Input must be numeric of character.")
    }
    return(ans)
}
