#' @title Spectral entropy
#'
#' @description
#'
#' These functions allow to calculate entropy measurements of an MS/MS
#' spectrum based on the metrics suggested by
#' Li et al. (https://doi.org/10.1038/s41592-021-01331-z).
#' Spectral entropy and normalized entropy are used to measure the complexity
#' of an spectra.
#' MassBank of North America (MoNA) defines spectra entropy as the intensity
#' weighted spectral peak number
#' (https://mona.fiehnlab.ucdavis.edu/documentation/entropy).
#' Additionally it is suggested to consider spectra with a normalized
#' entropy larger than 0.8, or a spectral entropy larger than 3 as low-quality spectra.
#'
#' @param x `numeric`, intensities of the fragment ions.
#'
#' @return `numeric`: (normalized) entropy of `x`.
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
#' entropy(spectrum[,2])
#' nentropy(spectrum[,2])
#'
entropy <- function(x) {
  y <- x / sum(x)
  -sum(y * log(y))
}

#' @rdname entropy
#'
#' @export
nentropy <- function(x) {
  entropy(x) / log(length(x))
}