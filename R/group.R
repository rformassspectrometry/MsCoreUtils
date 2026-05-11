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
#' value in a group can be larger than `tolerance` and `ppm`.
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







#---- New lines for adding new function for grouping based on the corresponding intensity decreasingly --#

#' @title Grouping of numeric values by similarity by considering element of intensity
#'
#' @description
#'
#' The `group_mz_int` function aims to group similar m/z across several scans.
#' it groups numeric values by first ordering based on intensity decreasingly, 
#' and then picking up the m/z with the highest intensity, putting
#' all values into the same group if their difference is smaller than defined by
#' parameters `tolerance` (a constant value) and `ppm` (a value-specific
#' relative value expressed in parts-per-million). 
#' If the number of mass within that group higher than the defined max_num (scans),
#' then the number of defined scans of top peaks would be kept. 
#' Interactively, do the above steps until the last m/z gettting the group id
#'
#' @note
#'
#' This method solves the scenario like the difference between the smallest and largest value
#' in a group can be larger than `tolerance` and `ppm`.
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
#' @author ctb (Muyao Xi)
#'
#' @rdname group_mz_int
#'
#' @export group_mz_int
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
#' group_mz_int(x, y, num_max, ppm = 10)
#'
#' ## Same on an unsorted vector
#' x <- c(56, 56.012, 56.016, 56.004, 56.008, 56.02)
#' y = c(52151, 58452, 458, 125584, 582, 57452)
#' group_mz_int(x, y, max_num, tolerance = 0.005)
#'
#' ## the same three groups were made.
#' 
#' @importFrom utils tail



group_mz_int <- function(x, y, max_num = Inf, tolerance = 0, ppm = 0) {
    #group m/z within tolerance based on the intensity decreasingly
    
    
    tolerance <- rep(tolerance + sqrt(.Machine$double.eps), length(x))
    if (ppm > 0)
        tolerance <- tolerance + ppm(x, ppm)
    
    
    

    mz <- x
    int <- y
    group_ids <- rep(0, length(x))
    index <- seq_along(x)
    
    group_id_zero <- which(group_ids == 0)
    group_id <- 0
    
    
    while (length(group_id_zero) != 0) {
        group_id <- group_id + 1
        id_int_max <- group_id_zero[which.max(int[group_id_zero])]
        

        logi_id <- abs(mz[group_id_zero]-mz[id_int_max])<=tolerance[group_id_zero]
        
        if (sum(logi_id) == 0) {
            group_ids[id_int_max] <- group_id
            
        } else if (is.finite(max_num) && sum(logi_id) > max_num) {
            

            int_logi <- int[group_id_zero[logi_id]]
            index_logi <- index[group_id_zero[logi_id]]
            
            index_sel_int <- order(int_logi)
            
            index_top_int <- tail(index_sel_int, max_num)
            index_0_int <- setdiff(index_sel_int, index_top_int)
            

            group_ids[index_logi[index_top_int]] <- rep(group_id, length(index_top_int))
            group_ids[index_logi[index_0_int]] <- rep(0, length(index_0_int))
            
            
        } else {
            group_ids[group_id_zero[logi_id]] <- rep(group_id, sum(logi_id))
        } 
        
        group_id_zero <- which(group_ids == 0)
        
        
    }
    
    res <- group_ids

    res
}

#------------------------------- New lines end ----------------------------##########