#' @title Check functions
#'
#' @description
#' These functions are used to check input arguments.
#'
#' @param x object to test.
#'
#' @return `logical(1)`, `TRUE` if checks are successful otherwise `FALSE`.
#'
#' @author Sebastian Gibb
#' @family helper functions for developers
#' @export
#' @examples
#' isPeaksMatrix(1:2)
#' isPeaksMatrix(cbind(mz = 1:2, intensity = 1:2))
isPeaksMatrix <- function(x) {
    isTRUE(
        is.matrix(x) &&
        mode(x) == "numeric" &&
        ncol(x) == 2L &&
        !is.null(colnames(x)) &&
        all(colnames(x) == c("mz", "intensity"))
    )
}
