#' @name graphPeaks
#' 
#' @title
#' Match two spectra using bipartite networks and combinatorics
#' 
#' @description
#' Function to match spectrum objects using bipartite networks and combinations
## to match peaks. `graphPeaks` takes two objects, `x` and 
#' `y` as input that contain spectral information. The matching 
#' is a multi-step procedure: 
#' 1) filtering based on `ppm`,
#' 2) retain order of matches between features of `x` and 
#' `y` (remove crossing edges that violate the order of matching
#' m/z),
#' 3) calculate all combinations of the remaining possibilities.  
#' 
#' @param
#' x `matrix`, the first column (`"mz"`) contains m/z value and the 
#' second column (`"intensity"`) contains the corresponding intensity values 
#'                         
#' @param
#' y `matrix`, the first column (`"mz"`) contains m/z value and the 
#' second column (`"intensity"`) contains the corresponding intensity values
#'                      
#' @param
#' ppm `numeric`, tolerance parameter in ppm to match corresponding peaks
#' 
#' @param 
#' FUN function to calculate similarity between spectra
#' 
#' @param
#' ... additional parameters passed to `FUN`
#' 
#' @details
#' Objective function is highest similarites between the two 
#' spectral objects, i.e. `FUN` is calculated over all combinations and 
#' the similarity of the combination that yields the highest similarity is 
#' returned. 
#' 
#' @return
#' list with elements `x` and `y` each being a matrix with columns `"mz"` 
#' and `"intensity"`. Each row (peak) in `x` matches the row (peak) in `y`
#' 
#' @author
#' Thomas Naake \email{thomasnaake@@googlemail.com}
#' 
#' @examples
#' x <- matrix(c(c(100.001, 100.002, 300.01, 300.02),
#'     c(1, 1, 1, 1)), ncol = 2, nrow = 4, byrow = FALSE)
#' colnames(spectrum1) <- c("mz", "intensity")
#' 
#' y <- matrix(c(c(100.0, 200.0, 300.002, 300.025, 300.0255),
#'     c(1, 1, 1, 1, 1)), ncol = 2, nrow = 5, byrow = FALSE)
#' colnames(spectrum2) <- c("mz", "intensity")
#' graphPeaks(x = x, y = y, ppm = 20, FUN = dotproduct, ...)
#' @export
graphPeaks <- function(x, y, ppm = 20, FUN = dotproduct, ...) {
    
    if (!is.matrix(x)) stop("x is not a matrix")
    if (!is.matrix(y)) stop("y is not a matrix")
    if (mode(x) != "numeric") stop("mode(x) is not 'numeric'")
    if (mode(y) != "numeric") stop("mode(y) is not 'numeric'")
    if (!all(colnames(x) %in% c("mz", "intensity"))) stop("colnames(x) are not 'mz' and 'intensity'")
    if (!all(colnames(y) %in% c("mz", "intensity")))stop("colnames(y) are not 'mz' and 'intensity'")
    if (!is.numeric(ppm)) stop("ppm is not numeric")
    if (ppm < 0) stop("ppm has to be positive")
    
    
    ## re-set colnames for x and y and order
    rownames(x) <- paste("sp1_", 1:nrow(x),sep = "")
    rownames(y) <- paste("sp2_", 1:nrow(y),sep = "")
    if (nrow(x) > 1) x <- x[order(x[, 1]), ]
    if (nrow(y) > 1) y <- y[order(y[, 1]), ]
    
    ## 1) create adjacency matrix and remove edges within x and 
    ## within y
    w <- matrix(1, ncol = nrow(x) + nrow(y), 
                nrow = nrow(x) + nrow(y),
                dimnames = list(c(rownames(x), rownames(y)),
                              c(rownames(x), rownames(y))
                ))
    
    ## 2) remove edges that are not in a certain range
    ## calculate upper and lower m/z based on ppm parameter
    ppm_1_1 <- x[, 1] / abs(ppm / 10 ^ 6  - 1)
    ppm_1_2 <- x[, 1] / abs(ppm / 10 ^ 6  + 1)
    ppm_2_1 <- y[, 1] / abs(ppm / 10 ^ 6  - 1)
    ppm_2_2 <- y[, 1] / abs(ppm / 10 ^ 6  + 1)
    
    ## set names 
    names(ppm_1_1) <- rownames(x)
    names(ppm_1_2) <- rownames(x)
    names(ppm_2_1) <- rownames(y)
    names(ppm_2_2) <- rownames(y)
    
    mat1 <- apply(as.matrix(ppm_1_2), 1, function(a) a <= c(ppm_1_1, ppm_2_1))
    mat2 <- apply(as.matrix(ppm_1_1), 1, function(a) a >= c(ppm_1_2, ppm_2_2))
    
    link_ppm <- mat1 * mat2
    w[rownames(link_ppm), colnames(link_ppm)] <- link_ppm
    w[colnames(link_ppm), rownames(link_ppm)] <- t(link_ppm)
    w[rownames(x), rownames(x)] <- 0
    w[rownames(y), rownames(y)] <- 0
    
    ## obtain network components from w
    net <- graph_from_adjacency_matrix(w, weighted = NULL, mode = "undirected")
    comp <- components(net)
    
    ## 3) get all possible combinations within one component
    ## res will contain all combinations within one component
    res <- vector("list", length(comp$csize)) 
    
    ## write to res combinations where csize == 2
    inds_1 <- which(comp$csize == 1)
    res[inds_1] <- lapply(inds_1, function(a) {
        ms <- names(which(comp$membership == a))
        if (grepl(x = ms, pattern = "sp1")) {
            ms <- c(ms, NA)
        } else{
            ms <- c(NA, ms)
        }
        matrix(ms, ncol = 2)
    })
    ## write to res combinations where csize == 2
    inds_2 <- which(comp$csize == 2)
    res[inds_2] <- lapply(inds_2, function(a) matrix(names(which(comp$membership == a)), ncol = 2))
    
    ## get combinations where csize > 2
    inds <- which(comp$csize > 2)
    
    for (i in inds) {
        
        ## separate component and create two matrices from x and 
        ## y that only contain component menbers
        names_ind_i <- names(which(comp$membership == i))
        x_ind <- x[names_ind_i[names_ind_i %in% rownames(x)], ]
        x_ind <- matrix(x_ind, ncol = 2,
                        dimnames = list(names_ind_i[names_ind_i %in% rownames(x)], c("", "")))
        y_ind <- y[names_ind_i[names_ind_i %in% rownames(y)], ]
        y_ind <- matrix(y_ind, ncol = 2,
                        dimnames = list(names_ind_i[names_ind_i %in% rownames(y)], c("", ""))) 
        
        ## allocate to c1 and c2 the names (colnames of x and y 
        ## that are in the specific component)
        if (nrow(x_ind) < nrow(y_ind)) {
            c1 <- rownames(x_ind); c2 <- rownames(y_ind)
            c1 <- c(c1, rep("NA", length(c2)-length(c1))) 
            c1_c2 <- lapply(c1, function(a) expand.grid(a, c2))
        } else {
            c1 <- rownames(y_ind); c2 <- rownames(x_ind)
            c1 <- c(c1, rep("NA", length(c2)-length(c1))) 
            c1_c2 <- lapply(c2, function(a) expand.grid(a, c1))
        }
        
        c1_c2_paste <- lapply(c1_c2, function(a) apply(a, 1, 
            function(b) paste(b, collapse = " & ")))
        
        ## calculate all possible combinations
        res_i <- as.matrix(expand.grid(c1_c2_paste, stringsAsFactors = FALSE))
        ## write rows to list
        res_i <- split(res_i, row(res_i))
        ## strsplit " & ", unlist and write to matrix
        res_i <- lapply(res_i, strsplit, split = " & ")
        res_i <- lapply(res_i, unlist)
        res_i <- matrix(unlist(res_i), nrow = length(res_i), byrow = TRUE)
        
        ## remove rows that contain duplicated values 
        res_i <- res_i[!apply(apply(res_i, 1, duplicated), 2, any), ]
        
        ## filtering for crossing matching: retain order of m/z
        seqs <- seq(2, ncol(res_i), by = 2)
        if (ncol(res_i) > 2) {
            ## do if there is only a multiple mapping:
            ## check order of sp2s, they have to ascend, remove those that
            ## do not ascend
            crosses <- lapply(
                as.data.frame(t(res_i[, seqs]), stringsAsFactors = FALSE),
                function(a) as.numeric(substr(a, 5, nchar(a))))
            ## check where all are TRUE, remove before NAs
            crosses <- lapply(crosses, function(a) {
                a <- a[!is.na(a)]
                all(a == sort(a))
            })
            res_i <- matrix(res_i[unlist(crosses), ], ncol = ncol(res_i))
            
            ## create shifted matrices, do not use last element since it contains
            ## only NAs
            shift_right <- lapply(seqs[-length(seqs)], 
                function(a) shiftMatrix(res_i, seqs, a / 2))
            shift_left <- lapply(seqs[-length(seqs)], 
                function(a) shiftMatrix(res_i, seqs, -a / 2))
            
            ## write to matrix
            mat_shift_left <- lapply(shift_left, function(a) {
                res_i[, seqs] <- a
                return(res_i)
            })
            mat_shift_right <- lapply(shift_right, function(a) {
                res_i[, seqs] <- a
                return(res_i)
            })
            
            ## rbind the lists
            mat_shift_left <- do.call(rbind, mat_shift_left)
            mat_shift_right <- do.call(rbind, mat_shift_right)
            
            ## the ones that are shifted out, link to NA and bind to NA
            ## for mat_shift_left (take from shift_right and inverse)
            add_left <- shift_right[length(shift_right):1]
            add_left <- lapply(add_left, function(x) x[, -1])
            
            mat_add_left <- mat_add_right <- matrix("NA", 
                ncol = length(shift_left) * 2, nrow = nrow(mat_shift_left))
            mat_add_left[, seqs[-length(seqs)]] <- unlist(add_left)
            
            ## for mat_shift_right (take from shift_left and inverse)
            add_right <- shift_left[length(shift_left):1]
            add_right <- lapply(add_right, 
                function(x) matrix(x[, ncol(x):1], ncol = ncol(x)))
            add_right <- lapply(add_right, function(x) x[, -1])
            mat_add_right[, seqs[-length(seqs)]] <- unlist(add_right)
            
            ## cbind with mat_add_left and mat_add_right
            mat_shift_left <- cbind(mat_shift_left, mat_add_left)
            mat_shift_right <- cbind(mat_shift_right, mat_add_right)
            
            ## add to res_i
            res_i <- cbind(res_i, matrix("NA", nrow = nrow(res_i), ncol = ncol(mat_add_left)))
            
            ## assign to res
            res[[i]] <- rbind(res_i, mat_shift_left, mat_shift_right)
        } else {
            res[[i]] <- res_i
        }
    }
    
    ## create combinations between rows
    res_paste <- lapply(res, function(a) apply(a, 1, paste, collapse = " & "))
    
    ## 4) calculate all possible combinations between the components
    res_exp <- as.matrix(expand.grid(res_paste, stringsAsFactors = FALSE))
    ## write rows to list
    res_exp <- split(res_exp, row(res_exp))
    ## strsplit " & ", unlist and write to matrix 
    res_exp <- lapply(res_exp, strsplit, split = " & ")
    res_exp <- lapply(res_exp, unlist)
    res_exp <- matrix(unlist(res_exp), nrow = length(res_exp), byrow = TRUE)
    
    ## 5) go through every row and calculate score: row with highest score is 
    ## the best match
    sim <- apply(res_exp, 1, function(a) {
        sp1_ind <- a[seq(1, ncol(res_exp), 2)]
        sp2_ind <- a[seq(2, ncol(res_exp), 2)]
        
        ## remove elements when two "NA" are at the same position 
        ## (they were created artificially in the above step when pushing 
        ## sp1/sp2 out and cbinding the pushed out from the matrix)
        ind_remove <- sp1_ind == "NA" & sp2_ind == "NA"
        sp1_ind <- sp1_ind[!ind_remove]
        sp2_ind <- sp2_ind[!ind_remove]
        
        ## create vectors that store mz and intensity of combination
        mz1 <- rep(NA, length(sp1_ind))
        int1 <- numeric(length(sp1_ind))
        mz2 <- rep(NA, length(sp2_ind))
        int2 <- numeric(length(sp2_ind))
        
        mz1[sp1_ind != "NA"] <- x[sp1_ind[sp1_ind != "NA"], "mz"]
        mz2[sp2_ind != "NA"] <- y[sp2_ind[sp2_ind != "NA"], "mz"]
        int1[sp1_ind != "NA"] <- x[sp1_ind[sp1_ind != "NA"], "intensity"]
        int2[sp2_ind != "NA"] <- y[sp2_ind[sp2_ind != "NA"], "intensity"]
        
        sp1 <- data.frame(mz = mz1, intensity = int1)
        sp2 <- data.frame(mz = mz2, intensity = int2)
        
        ## get similarity value from function FUN
        value <- FUN(sp1, sp2, ...)
        
        list(value = value, x = sp1, y = sp2)
        
    })
    sim_value <- unlist(lapply(sim, "[[", "value"))
    sim_max <- sim[[which.max(sim_value)]]
    
    # ## sort according to ascending order
    x_max <- as.matrix(sim_max[["x"]])
    y_max <- as.matrix(sim_max[["y"]])
    
    sort_x_y <- lapply(seq_len(nrow(x_max)), function(a)
        paste(sort(c(x_max[a, "mz"], y_max[a, "mz"])), collapse = "_")
    )
    sort_x_y <- order(unlist(sort_x_y))
    x_max <- x_max[sort_x_y, ]
    y_max <- y_max[sort_x_y, ]
    
    list(x = x_max, y = y_max)
    
}

#' @name shiftMatrix
#' @title Shift columns of a matrix by n and set added columns to def
#' @description `shiftMatrix` shifts columns of a matrix by `n` and 
#' sets the added columns to `def`.
#' @usage shiftMatrix(mat, x, n, def = NA)
#' @param mat `matrix`
#' @param x `numeric`, col indices to shift
#' @param n `numeric(1)`, gives the number by how many positions the columns 
#' `x` of `mat` are shifted
#' @param def `character(1)`/`numeric(1)`, replacement value for added columns
#' @details helper function for `graphPeaks`
#' @return matrix with all combinations of shifted rows, only returns the 
#' columns `x` of `mat`
#' @author Thomas Naake \email{thomasnaake@@googlemail.com}
#' @examples 
#' mat <- matrix(letters[1:18], ncol = 6, nrow = 3)
#' x <- c(2, 4, 6)
#' shiftMatrix(mat = mat, x = x, n = 1, def = NA)
#' @export
shiftMatrix <- function(mat, x, n, def = NA){
    if (n == 0) { res <- mat }
    if (n < 0) {
        n <- abs(n)
        res <- mat[, x[ seq_len(length(x) - n) + n ]]
        res <- cbind(matrix(res, nrow = nrow(mat), byrow = FALSE),
                    matrix(def, nrow = nrow(mat), ncol = n))
    } else {
        res <- mat[, x[ seq_len(length(x) - n) ]]
        res <- cbind(matrix(def, nrow = nrow(mat), ncol = n),
                    matrix(res, nrow(mat), byrow = FALSE))
    }
    return(res)
}

