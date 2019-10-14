#' @title Check functions
#'
#' @description
#' These functions are used to check input arguments.
#'
#' @details
#' `isPeaksMatrix`: test for a `numeric` matrix with two columns named "mz" and
#' "intensity". The "mz" column has to be sorted increasingly.
#'
#' @param x object to test.
#'
#' @return `logical(1)`, `TRUE` if checks are successful otherwise `FALSE`.
#'
#' @author Sebastian Gibb
#' @family helper functions for developers
#' @rdname check
#' @export
#' @examples
#' isPeaksMatrix(1:2)
#' isPeaksMatrix(cbind(mz = 2:1, intensity = 1:2))
#' isPeaksMatrix(cbind(mz = 1:2, intensity = 1:2))
isPeaksMatrix <- function(x) {
    isTRUE(
        is.matrix(x) &&
        mode(x) == "numeric" &&
        ncol(x) == 2L &&
        !is.null(colnames(x)) &&
        all(colnames(x) == c("mz", "intensity")) &&
        !is.unsorted(x[, 1L], na.rm = TRUE)
    )
}
