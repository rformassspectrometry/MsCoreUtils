##' @title Check if a certain neutral loss occured
##'
##' @description
##' This checks if m/z value in a fragmentation spectrum is similar to a m/z
##' obtained from a neutral loss. The product ion m/z is calculated from the 
##' arguments `precursorMz` and `neutralLoss`
##'
##' @param x `matrix` with two column where one contains m/z values 
##' (column `"mz"`) and the second corresponding intensity values
##'  (column `"intensity"`)
##' @param precursorMz `numeric(1)` Precursor m/z from which the neutral loss is 
##' occuring
##' @param neutralLoss `numeric(1)` Mass of neutral loss. Will be subtracted from 
##' `precursorMz`
##' @param tolerance `numeric(1)`, accepted tolerance.
##' @param ppm `numeric(1)` representing a relative, value-specific
##'  parts-per-million (PPM) tolerance that is added to `tolerance`.
##'  
##' @return `logical` Returns either TRUE or FALSE, if fitting fragment m/z was
##'  found or not
##' 
##' @author Michael Witting
##'
##' @family Spectral analysis
##' 
##' @export
##' 
##' @examples
##'x <- matrix(c(c(95.08628, 97.10201, 123.11601, 240.26794, 252.26850,
##'                270.27852, 590.58606, 608.59827, 609.60059, 626.60829,
##'                627.61158, 628.61115),
##'              c(344, 454, 374, 268, 1206,
##'                996, 256, 2032, 1026, 7270,
##'                2386, 298)),  ncol = 2,)
##'                
##'colnames(x) <- c("mz", "intensity")
##'
##'containsNeutralLoss(x,
##'                    precursorMz = 626.60816, neutralLoss = 18.010565,
##'                    tolerance = 0.005)
containsNeutralLoss <- function(x,
                       precursorMz = 0, neutralLoss = 0,
                       tolerance = 0, ppm = 0) {
  
  ## check valid input
  validPeaksMatrix(x)
  
  # get mz values and check only for small values
  mz <- x[, "mz"]
  mz <- mz[which(mz < precursorMz)]
  
  # calculate fragment mass that would result from neutral loss
  fragMz <- precursorMz - neutralLoss
  
  # check if such a mass is found in the mz values
  match <- closest(mz, fragMz, tolerance = tolerance, ppm = ppm)
  
  # check if this contains any
  any(!is.na(match))
  
}
##' @title Check if a certain specified product ions can be found
##'
##' @description
##' This checks if one or multiple fragment m/z values are present in a
##' fragmentation spectrum
##'
##' @param x `matrix` with two column where one contains m/z values 
##' (column `"mz"`) and the second corresponding intensity values
##'  (column `"intensity"`)
##' @param productMz `numeric` One or multiple fragment m/z that shall be present
##' @param tolerance `numeric(1)`, accepted tolerance.
##' @param ppm `numeric(1)` representing a relative, value-specific
##'  parts-per-million (PPM) tolerance that is added to `tolerance`.
##'  
##' @return `logical` Returns either TRUE or FALSE, if fitting fragment m/z was
##'  found or not
##' 
##' @author Michael Witting
##'
##' @family Spectral analysis
##' 
##' @export
##' 
##' @examples
##' x <- matrix(c(c(238.25334, 250.25265, 251.25765, 268.26535, 606.58326,
##'                 607.58785, 624.59306, 625.59769, 626.60180),
##'               c(1088, 3852, 212, 200, 2178,
##'                 724, 1266, 584, 222)),  ncol = 2,)
##'                 
##'colnames(x) <- c("mz", "intensity")
##'
##'result <- containsProductIon(x,
##'                             productMz = c(250.2529, 268.2635, 238.2530),
##'                             tolerance = 0.005)
containsProductIon <- function(x,
                               productMz = 0,
                               tolerance = 0, ppm = 0) {
  
  ## check valid input
  validPeaksMatrix(x)
  
  # get mz values and check only for small values
  mz <- x[, "mz"]
  
  # sort productMz
  productMz <- sort(productMz)
  
  # check if such a mass is found in the mz values
  match <- closest(mz, productMz, tolerance = tolerance, ppm = ppm)
  
  # check if this contains any
  any(!is.na(match))
  
}
