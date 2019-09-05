#' @title Input parameter check for subsetting operations
#'
#' @description
#'
#' `i2index` is a simple helper function to be used in subsetting functions. It
#' checks and converts the parameter `i`, which can be of type `integer`,
#' `logical` or `character` to integer vector that can be used as index for
#' subsetting.
#'
#' @param i `character` `logical` or `integer` used in `[i]` for subsetting.
#'
#' @param length `integer` representing the `length` of the object to be
#'     subsetted.
#'
#' @param names `character` with the names (rownames or similar) of the object.
#'     This is only required if `i` is of type `character`.
#'
#' @return `integer` with the indices
#'
#' @author Johannes Rainer
#'
#' @export
#'
#' @examples
#'
#' ## With `i` being an `integer`
#' i2index(c(4, 1, 3), length = 10)
#'
#' ## With `i` being a `logical`
#' i2index(c(TRUE, FALSE, FALSE, TRUE, FALSE), length = 5)
#'
#' ## With `i` being a `character`
#' i2index(c("b", "a", "d"), length = 5, names = c("a", "b", "c", "d", "e"))
i2index <- function(i, length = 0L, names = NULL) {
    if (is.character(i)) {
        if (!length(names))
            stop("can not subset by name, object does not have names")
        if (!all(i %in% names))
            stop("not all names present in object")
        i <- match(i, names)
    } else if (is.logical(i)) {
        if (length(i) != length)
            stop("if i is logical it has to match the length of the object (",
                 length, ")")
        i <- which(i)
    } else if (is.numeric(i))
        i <- as.integer(i)

    if (is.integer(i) && !all(abs(i) %in% seq_len(length)))
        stop("index out of bounds: index has to be between 1 and ", length)
    i
}
