##' Converts seconds to/from "min:sec" format.
##'
##' @title Format Retention Time
##'
##' @param rt The retention time, either a `numeric()` in seconds or a
##'     `character()` as `"mm:ss"`.
##' 
##' @return A reformatted retention time.
##' 
##' @author Laurent Gatto
##'
##' @examples
##' formatRt(1524)
##' formatRt(1)
##' formatRt("25:24")
formatRt <- function(rt) {
    ans <- NA
    if (is.numeric(rt)) {
        min <- floor(rt / 60)
        sec <- round(rt - (min * 60))
        sec <- sprintf("%02d", sec)
        ans <- paste(min, ":", sec, sep = "")        
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

