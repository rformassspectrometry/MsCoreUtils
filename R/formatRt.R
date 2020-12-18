##' @export
##'
##' @rdname formatRt
##'
##' @examples
##'
##' ## rt2numeric
##'
##' rt2numeric("25:24")
##' rt2numeric(c("25:24", "25:25", "25:26"))
rt2numeric <- function(rt) {
if (!is.character(rt) || !length(rt))
    stop("'rt' has to be a character of length > 0.")
    vapply1d(strsplit(rt, ":"),
           function(x) {
               x <- as.numeric(x)
               60 * x[1] + x[2]
           })
}

##' @export
##'
##' @rdname formatRt
##'
##' @examples
##'
##' ## rt2character
##'
##' rt2character(1524)
##' rt2character(1)
##' rt2character(1:10)
rt2character <- function(rt) {
if (!is.numeric(rt) || !length(rt))
    stop("'rt' has to be a numeric of length > 0.")
    min <- floor(rt / 60)
    sec <- round(rt - (min * 60))
    sprintf("%d:%02d", min, sec)
}


##' Format Retention Time
##'
##' These vectorised functions convert retention times from a numeric
##' in seconds to/from a character as "mm:ss". `rt2character()`
##' performs the numeric to character conversion while `rt2numeric()`
##' performs the character to numeric conversion. `formatRt()` does
##' one of the other depending on the input type.
##'
##' @param rt A vector of retention times of length > 1. Either a
##'     `numeric()` in seconds or a `character()` as `"mm:ss"`
##'     depending on the function.
##'
##' @return A reformatted retention time.
##'
##' @author Laurent Gatto
##'
##' @rdname formatRt
##'
##' @aliases rt2character rt2numeric
##'
##' @export
##'
##' @examples
##'
##' ## formatRt
##'
##' formatRt(1524)
##' formatRt(1)
##' formatRt(1:10)
##' formatRt("25:24")
##' formatRt(c("25:24", "25:25", "25:26"))
formatRt <- function(rt) {
    if (is.numeric(rt)) return(rt2character(rt))
    if (is.character(rt)) return(rt2numeric(rt))
    stop("Input must be a character or a numeric.")
}
