#' @title Spectral entropy
#'
#' @description
#'
#' These functions allows to calculate entropy measurements of an MS/MS  
#' spectrum based on the metrics suggested by 
#' Li et al. (https://doi.org/10.1038/s41592-021-01331-z).
#'
#' @param x `numeric`, intensities of the fragment ions value(s).
#'
#' @return `numeric`: (normalized) entropy `x`.
#'
#' @author Mar Garcia-Aloy
#'
#' @family helper functions for users
#' 
#' #' @references
#' Li, Y., Kind, T., Folz, J., Vaniya, A., Mehta. S.S., Fiehn, O. (2021).
#' Spectral entropy outperforms MS/MS dot product similarity for 
#' small-molecule compound identification.
#' Nature Methods. 2021;18(12):1524-1531.
#' \doi{10.1038/s41592-021-01331-z}.
#'
#' @export
#'
#' @examples
#' spectrum <- rbind(c(41.04, 37.16), c(69.07, 66.83), c(86.1, 999.0))
#'
#' entropy_spectral(spectrum[,2])
#' entropy_normalized(spectrum[,2])
#' 
entropy_spectral <- function(x){
  y <- x / sum(x)
  z <- y*log(y)
  return(-sum(z))
}

entropy_normalized <- function(x){
  entropy_spectral(x) / log(length(x))
}