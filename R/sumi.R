#' @export
sumi <- function(x) {
    .Call(C_sumi, as.double(x))
}

#' @export
sumir <- function(x) {
    if (all(is.na(x)))
        NA_real_
    else sum(x, na.rm = TRUE)
}
