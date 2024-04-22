#' @title Extract the common file path
#'
#' @description
#'
#' Find the common part of the path for a provided set of files.
#'
#' @param x `character` with the file names (including paths).
#'
#' @param fsep `character(1)` defining the file separator to be used in
#'     the returned common path. Defaults to the system platform's file
#'     separator.
#'
#' @return `character(1)` representing the path common to all files in `x`.
#'
#' @author Johannes Rainer
#'
#' @export
#'
#' @note
#'
#' This function uses `"(\\\\)|/"` to split the provided paths into the
#' individual directories to support both Windows-specific and
#' unix-specific separators between folders. File and folder names
#' should thus **not** contain these characters.
#'
#' @examples
#'
#' ## Find the common part of the file path
#' pths <- c("/tmp/some/dir/a.txt", "/tmp/some/dir/b.txt",
#'     "/tmp/some/other/dir/c.txt", "/tmp/some/other/dir/d.txt")
#'
#' common_path(pths)
#'
#' ## If there is no common part
#' common_path(c("/a/b", "b"))
#'
#' ## Windows paths; note that "/" is used as file separator in the result
#' common_path(c("C:\\some\\path\\a.txt", "C:\\some\\path\\b.txt"))
#'
#' ## No input
#' common_path(character())
#'
#' ## No path
#' common_path(c("a.txt", "b.txt"))
common_path <- function(x, fsep = .Platform$file.sep) {
    if (!length(x))
        return(character())
    sx <- strsplit(x, split = "(\\\\)|/")
    minl <- min(lengths(sx))
    cpath <- character()
    for (i in seq_len(minl)) {
        uvals <- unique(vapply(sx, `[`, character(1), i = i))
        if (length(uvals) == 1L)
            cpath <- c(cpath, uvals)
    }
    paste0(cpath, collapse = fsep)
}
