#' @title Tools to reduce memory size of DataFrames
#'
#' @description
#'
#' These functions help reducing the memory requirement of [DataFrame()] objects
#' by converting columns with constant values to type [Rle()].
#'
#' - `asRle`: convert a `numeric`, `logical` or `character` vector into an
#'   [Rle()] if it will use less memory than the bare vector. Input arguments of
#'   a different type are returned as-is.
#'
#' - `asRleDataFrame`: convert columns in `x` (a [DataFrame()]) to type `Rle`
#'   (with `asRle`). Argument `columns` allows to specify columns that **have**
#'   to be converted (even if it would not reduce the memory demand).
#'
#' - `asVectorDataFrame`: convert all columns of type `Rle` in `x` back to base
#'   vectors (using `as.vector`).
#'
#' @param x For `asRle`: `numeric`, `logical` or `character` vector to convert.
#'
#' @param columns For `asRleDataFrame`: `character` with optional column names
#'     that should always be converted to type `Rle`.
#'
#' @author Johannes Rainer, Sebastian Gibb
#' @family coerce functions
#' @name coerce-rle
NULL

#' @name coerce-rle
#'
#' @importFrom S4Vectors Rle nrun
#'
#' @export
asRle <- function(x) {
    if (length(x) > 2L && (is.numeric(x) || is.character(x) || is.logical(x))) {
        r <- Rle(x)
        if (nrun(r) < length(x) / 2L)
            return(r)
    }
    x
}

#' @name coerce-rle
#'
#' @export
asRleDataFrame <- function(x, columns = character()) {
    if (nrow(x) <= 1)
        return(x)
    for (col in colnames(x)) {
        x[[col]] <- asRle(x[[col]])
    }
    columns <- intersect(columns, colnames(x))
    for (col in columns) {
        if (!is(x[[col]], "Rle"))
            x[[col]] <- Rle(x[[col]])
    }
    x
}

#' @name coerce-rle
#'
#' @importClassesFrom S4Vectors Rle
#'
#' @importFrom methods is
#'
#' @export
asVectorDataFrame <- function(x) {
    x@listData <- lapply(x, function(col) {
        if (is(col, "Rle"))
            as.vector(col)
        else
            col
    })
    x
}
