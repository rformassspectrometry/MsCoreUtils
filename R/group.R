#' @title Grouping of numeric values by similarity
#'
#' @description
#'
#' The `group` function groups numeric values by first ordering and then putting
#' all values into the same group if their difference is smaller defined by
#' parameters `tolerance` (a constant value) and `ppm` (a value-specific
#' relative value expressed in parts-per-million).
#'
#' @note
#'
#' Since grouping is performed on pairwise differences between consecutive
#' values (after ordering `x`), the difference between the smallest and largest
#' value in a group can be larger than `tolerance` and `ppm`. See
#' [group_mz_int()] for an alternative that ensures all peaks within a group
#' have an m/z closer than defined by `tolerance` and `ppm`.
#'
#' @param x increasingly ordered `numeric` with the values to be grouped.
#'
#' @param tolerance `numeric(1)` with the maximal accepted difference between
#'     values in `x` to be grouped into the same entity.
#'
#' @param ppm `numeric(1)` defining a value-dependent maximal accepted
#'     difference between values in `x` expressed in parts-per-million.
#'
#' @return `integer` of length equal to `x` with the groups.
#'
#' @author Johannes Rainer, Sebastin Gibb
#'
#' @rdname group
#'
#' @export group
#'
#' @seealso [group_mz_int()]
#'
#' @examples
#'
#' ## Define a (sorted) numeric vector
#' x <- c(34, 35, 35, 35 + ppm(35, 10), 56, 56.05, 56.1)
#'
#' ## With `ppm = 0` and `tolerance = 0` only identical values are grouped
#' group(x)
#'
#' ## With `tolerance = 0.05`
#' group(x, tolerance = 0.05)
#'
#' ## Also values 56, 56.05 and 56.1 were grouped into a single group,
#' ## although the difference between the smallest 56 and largest value in
#' ## this group (56.1) is 0.1. The (pairwise) difference between the ordered
#' ## values is however 0.05.
#'
#' ## With ppm
#' group(x, ppm = 10)
#'
#' ## Same on an unsorted vector
#' x <- c(65, 34, 65.1, 35, 66, 65.2)
#' group(x, tolerance = 0.1)
#'
#' ## Values 65, 65.1 and 65.2 have been grouped into the same group.
group <- function(x, tolerance = 0, ppm = 0) {
    if (is.unsorted(x)) {
        idx <- order(x)
        x <- x[idx]
    } else idx <- integer()
    tolerance <- tolerance + sqrt(.Machine$double.eps)
    if (ppm > 0)
        tolerance <- tolerance + ppm(x[-length(x)], ppm)
    res <- cumsum(c(1L, diff(x) >= tolerance))
    res[idx] <- res
    res
}

#' @title Grouping of numeric values by similarity of m/z and intensity
#'
#' @description
#'
#' The `group_mz_int()` function groups peaks with similar m/z (across several
#' scans) considering also their intensity. The algorithm first orders peak
#' decreasingly by their intensity. Then it iteratively selects the peak with
#' the highest intensity that is not yet part of a peak group and finds all
#' other peaks with a difference in their m/z that is smaller than defined by
#' `tolerance` and `ppm`. These peaks are assigned to the same peak group.
#'
#' Setting parameter `max_num` to a finite number forces each peak group to
#' contain only the at most `max_num` peaks ordered by their intensity.
#'
#' @note
#'
#' This method solves the scenario like the difference between the smallest and
#' largest value in a group can be larger than `tolerance` and `ppm`.
#'
#' @param x `numeric` with the *m/z* values to be grouped.
#'
#' @param y `numeric` with the intensity values of the peaks.
#'
#' @param max_num `integer(1)` defining the maximum number of peaks for a peak
#'     group.
#'
#' @param tolerance `numeric(1)` with the maximal accepted difference between
#'     values in `x` to be grouped into the same entity.
#'
#' @param ppm `numeric(1)` defining a value-dependent maximal accepted
#'     difference between values in `x` expressed in parts-per-million.
#'
#' @return `integer` of length equal to `x` with the groups.
#'
#' @author Muyao Xi
#'
#' @rdname group_mz_int
#'
#' @export group_mz_int
#'
#' @seealso [group()]
#'
#' @examples
#'
#' ## Define a (sorted) numeric vector
#' x = c(56, 56.004, 56.008, 56.012, 56.016, 56.02)
#' y = c(52151, 125584, 582, 58452, 458, 57452)
#' max_num = 2
#'
#' ## With `ppm = 0` and `tolerance = 0` only identical values are grouped
#' group_mz_int(x, y, max_num)
#'
#' ## With `tolerance = 0.005`
#' group_mz_int(x, y, max_num, tolerance = 0.005)
#'
#' ## three groups were made.
#'
#' ## With ppm
#' group_mz_int(x, y, max_num, ppm = 10)
#'
#' ## Same on an unsorted vector
#' x <- c(56, 56.012, 56.016, 56.004, 56.008, 56.02)
#' y = c(52151, 58452, 458, 125584, 582, 57452)
#' group_mz_int(x, y, max_num, tolerance = 0.005)
#'
#' ## the same three groups were made.
#'
#' @importFrom utils tail
group_mz_int <- function(x, y = numeric(), max_num = Inf, tolerance = 0.0,
                         ppm = 0.0) {
    lx <- length(x)
    tolerance <- rep(tolerance + sqrt(.Machine$double.eps), lx)
    if (ppm > 0)
        tolerance <- tolerance + ppm(x, ppm)
    if (lx != length(y))
        stop("Length of 'x' and 'y' must match")
    mz <- x
    int <- y
    group_ids <- rep(0, lx)
    index <- seq_along(x)
    group_id_zero <- which(group_ids == 0L)
    group_id <- 0L
    while (length(group_id_zero) != 0) {
        group_id <- group_id + 1L
        id_int_max <- group_id_zero[which.max(int[group_id_zero])]
        logi_id <- abs(mz[group_id_zero] - mz[id_int_max]) <=
            tolerance[group_id_zero]
        if (sum(logi_id) == 0) {
            group_ids[id_int_max] <- group_id
        } else if (is.finite(max_num) && sum(logi_id) > max_num) {
            int_logi <- int[group_id_zero[logi_id]]
            index_logi <- index[group_id_zero[logi_id]]
            index_sel_int <- order(int_logi)
            index_top_int <- tail(index_sel_int, max_num)
            index_0_int <- setdiff(index_sel_int, index_top_int)
            group_ids[index_logi[index_top_int]] <- rep(group_id,
                                                        length(index_top_int))
            group_ids[index_logi[index_0_int]] <- rep(0L, length(index_0_int))
        } else {
            group_ids[group_id_zero[logi_id]] <- rep(group_id, sum(logi_id))
        }
        group_id_zero <- which(group_ids == 0)
    }
    group_ids
}
