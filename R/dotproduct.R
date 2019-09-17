#' @title Calculate the normalized dot product
#' 
#' @description 
#' Calculate the normalized dot product (NDP).`dotproduct` returns a numeric 
#' value ranging between 0 and 1, where 0 indicates no similarity between the 
#' two MS/MS features, while 1 indicates that the MS/MS features are identical. 
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
#' Each row in `x` corresponds to the respective row in `y`, i.e. the peaks 
#' (entries `"mz"`) per spectrum have to match.
#' `m` and `n` are weights given on the peak intensity and the m/z values 
#' respectively. As default (`m = 0.5`), the square roots of the intensity 
#' values are taken to calculate weights. With increasing values for `m`, high
#' intensity values will be taken more into account for similarity calculation, 
#' i.e. differences between intensities will be intensified. 
#' With increasing values for `n`, high m/z values will be taken more into 
#' account for similarity calculation. Especially when working with small 
#' molecules, a value `n > 0` can be set, to give a weight on the m/z values to 
#' accommodate that shared fragments with higher m/z are less likely and will 
#' mean that molecules might be more similar. If `n != 0`, a warning will be 
#' raised if the corresponding m/z values are not identical, since small 
#' differences in m/z values will distort the similarity values with increasing
#' `n`. If `m=0` or `n=0`, intensity values or m/z values, respectively, are not 
#' taken into account.
#' 
#' The normalized dot product is calculated according to: 
#' \deqn{NDP = \frac{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2}{ \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2) }}{\sum(W_{S1, i} \cdot W_{S2, i}) ^ 2 \sum(W_{S1, i} ^ 2) * \sum(W_{S2, i} ^ 2)},
#' with \eqn{W = [ peak intensity] ^{m} \cdot [m/z]^n}. 
#' For further information on normalized dot product see for example
#' Li et al. (2015).
#' Prior to calculating \deqn{W_{S1}} or \deqn{W_{S2}}, all intensity values 
#' are divided by the maximum intensity value. 
#' 
#' @references 
#' Li et al. (2015): Navigating natural variation in herbivory-induced
#' secondary metabolism in coyote tobacco populations using MS/MS structural 
#' analysis. PNAS, E4147--E4155.
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
#' dotproduct(x, y, m = 0.5, n = 0) 
#' 
#' @export
dotproduct <- function(x, y, m = 0.5, n = 0) {
    
    ## check valid input 
    if (!is.list(x)) stop("'x' is not a list")
    if (!is.list(y)) stop("'y' is not a list")
    if (!is.numeric(m)) stop("`m` is not numeric")
    if (length(m) != 1) stop("`m` has to be of length 1")
    if (!is.numeric(n)) stop("`n` is not numeric")
    if (length(n) != 1) stop("`n` has to be of length 1")
        
    ## retrieve m/z and intensity from x and y
    mz1 <- x$mz
    mz2 <- y$mz
    inten1 <- x$intensity
    inten2 <- y$intensity
    
    ## check mz1, inten1, mz2 and inten2
    if (!all(is.numeric(mz1))) stop("x$mz is not numeric")
    if (!all(is.numeric(mz2))) stop("y$mz is not numeric")
    if (!all(is.numeric(inten1))) stop("x$intensity is not numeric")
    if (!all(is.numeric(inten2))) stop("y$intensity is not numeric")
    
    if (length(mz1) != length(inten1)) {
        stop("length(x$mz) not equal to length(x$intensity)")
    }    
    if (length(mz1) != length(mz2)) {
        stop("length(x$mz) not equal to length(y$mz)")
    }
    if (length(mz1) != length(inten2)) {
        stop("length(x$mz) not equal to length(y$inten)")
    }
    
    ## check mz values: if mz1 and mz2 are not identical and the values are
    ## weighted by n, this might to unexpected results in the similarity
    ## calculation
    na_ind <- is.na(mz1) | is.na(mz2)
    if (!all(mz1[ !na_ind ] == mz2[ !na_ind ])) {
        if (n != 0) {
            warning("m/z values in x are not identical to m/z values in y.",
            "If n != 0 this might lead to unexpected results.")    
        }
    }
    
    ## normalize to % intensity
    inten1 <- inten1 / max(inten1, na.rm = TRUE) * 100
    inten2 <- inten2 / max(inten2, na.rm = TRUE) * 100
    
    ws1 <- inten1 ^ m * mz1 ^ n
    ws2 <- inten2 ^ m * mz2 ^ n
    
    ## calculate dot product or normalized dot product respectively
    dp <- sum(ws1 * ws2, na.rm = TRUE) 
    dp ^ 2 / (sum(ws1 ^ 2, na.rm = TRUE) * sum(ws2 ^ 2, na.rm = TRUE))
}
