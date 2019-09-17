#' @name dotproduct
#' 
#' @title Calculate the normalized dot product
#' 
#' @description 
#' Calculate the normalized dot product (NDP).`dotproduct` returns a numeric 
#' value ranging between 0 and 1, where 0 indicates no similarity between the 
#' two MS/MS features, while 1 indicates that the MS/MS features are identical. 
#' 
#' @usage dotproduct(x, y, m=0.5, n=2)
#' 
#' @param 
#' x `list`/`data.frame` of length 2 with m/z (`"mz"`) and corresponding 
#' intensity values (`"intensity"`)
#' 
#' @param 
#' y `list`/`data.frame` of length 2 with m/z (`"mz"`) and corresponding 
#' intensity values (`"intensity"`)
#' 
#' @param m `numeric(1)`, exponent for peak intensity-based weights
#' 
#' @param n `numeric(1)`, exponent for m/z-based weights
#' 
#' @details 
#' `x` and `y` have to be spectrally aligned. Each row in `x` corresponds to 
#' the respective row in `y`. The dot product is calculated according to the 
#' following formula: \deqn{NDP = \sum(W_{S1, i} \cdot W_{S2, i})}, 
#' while the the normalized dot product is calculated according to: 
#' \deqn{NDP = \frac{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2}{ \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2) }}{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2 \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2)},
#' with \eqn{W = [ peak intensity] ^{m} \cdot [m/z]^n}. 
#' For further information on the dot product see for example
#' Toprak et al. (2014): Conserved Peptide Fragmentation as a Benchmarking 
#' Tool for Mass Spectrometers and a Discriminating Feature for Targeted 
#' Proteomics. Molecular \& Cellular Proteomics, 2056--2071.   
#' For further information on normalized dot product see for example
#' Li et al. (2015): Navigating natural variation in herbivory-induced
#' secondary metabolism in coyote tobacco populations using MS/MS structural 
#' analysis. PNAS, E4147--E4155.
#' Prior to calculating \deqn{W_{S1}} or \deqn{W_{S2}}, all intensity values 
#' are divided by the maximum intensity value. 
#' 
#' @return 
#' `numeric(1)`, `dotproduct` returns a numeric similarity coefficient between 
#' 0 and 1.
#' 
#' @author Thomas Naake, \email{thomasnaake@@googlemail.com}
#' 
#' @examples 
#' x <- data.frame(mz=c(100.002, 100.001, NA, 300.01, 300.02, NA), 
#'         intensity=c(2, 1.5, 0, 1.2, 0.9, 0))
#' y <- data.frame(mz = c(100.0, NA, 200.0, 300.002, 300.025, 300.0255),
#'         intensity = c(2, 0, 3, 1, 4, 0.4))
#' dotproduct(x, y, m = 0.5, n = 2) 
#' 
#' @export
dotproduct <- function(x, y, m = 0.5, n = 2) {
    
    if (!is.list(x)) stop("'x' is not a list")
    if (!is.list(y)) stop("'y' is not a list")
    
    ## retrieve m/z and intensity from x and y
    mz1 <- x$mz
    mz2 <- y$mz
    inten1 <- x$intensity
    inten2 <- y$intensity
    
    if (length(mz1) != length(mz2)) {
        stop("length(mz1) not equal to length(mz2)")
    }
    if (length(inten1) != length(mz2)) {
        stop("length(mz1) not equal to length(mz2)")
    }
    if (length(mz1) != length(inten1)) {
        stop("length(mz1) not equal to length(inten1)")
    }
    
    ## normalize to % intensity
    inten1 <- inten1 / max(inten1, na.rm = TRUE) * 100
    inten2 <- inten2 / max(inten2, na.rm = TRUE) * 100
    
    ws1 <- inten1 ^ m * mz1 ^ n
    ws2 <- inten2 ^ m * mz2 ^ n
    
    ## calculate dot product or normalized dot product respectively
    dp <- sum( ws1*ws2, na.rm = TRUE) 
    dp ^ 2 / ( sum( ws1^2, na.rm = TRUE) * sum( ws2^2, na.rm = TRUE ) )
}