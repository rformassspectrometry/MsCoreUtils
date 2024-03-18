#' @title Forcing a numeric vector into a monotonously increasing sequence.
#'
#' @description
#' This function performs interpolation on the non-increasing parts of an
#' input vector to ensure its values are continuously increasing.
#'
#' @param x `numeric` vector.
#'
#' @return A vector with continuously increasing values.
#'
#' @note
#' NA values will not be replaced.
#'
#' @examples
#' x <- c(NA, NA, NA, 1.2, 1.1, 1.14, 1.2, 1.3, NA, 1.04, 1.4, 1.6, NA, NA)
#' sorted_rtime <- force_sorted(x)
#' is.unsorted(x, na.rm = TRUE)
#'
#' @export
#'
#' @rdname force_sorted
force_sorted <- function(x){
    # Select only the non-NA values
    if (!is.numeric(x) && !is.integer(x))
        stop("'x' needs to be numeric or integer")
    nna_idx <- which(!is.na(x))
    vec_temp <- x[nna_idx]

    while (any(diff(vec_temp) < 0)) {
        idx <- which.max(diff(vec_temp) < 0)
        # Find next biggest value
        next_idx <- which(vec_temp > vec_temp[idx])[1L]

        if (is.na(next_idx)){
            l <- idx:length(vec_temp)
            vec_temp[l] <- seq(vec_temp[idx], by = 0.000001,
                               length.out = length(l))
            warning("Found decreasing values at the end of vector, ",
                    "interpolation not possible. Replacing values. See help ",
                    "for more details")
            break
        }
        # Interpolation
        idx_range <- idx:next_idx
        vec_temp[idx_range] <- seq(vec_temp[idx], vec_temp[next_idx],
                                   length.out = length(idx_range))
    }
    x[nna_idx] <- vec_temp
    x
}

