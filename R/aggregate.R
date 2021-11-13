##' @title Aggreagate quantitative features
##'
##' @description
##'
##' These functions take a matrix of quantitative features `x` and
##' aggregate the features (rows) according to either a vector (or
##' factor) `INDEX` or an adjacency matrix `MAT`. The aggregation
##' method is defined by function `FUN`.
##'
##' Adjacency matrices are an elegant way to explicitly encode for
##' shared peptides (see example below) during aggregation. Note
##' however that missing values can't generally be ignore using `na.rm
##' = TRUE`, as with vector-based aggregation (see examples below).
##'
##' @section Vector-based aggregation functions:
##'
##' When aggregating with a vector/factor, user-defined functions
##' must return a vector of length equal to `ncol(x)` for each level
##' in `INDEX`. Examples thereof are:
##'
##' - [medianPolish()] to fits an additive model (two way
##'   decomposition) using Tukey's median polish_ procedure using
##'   [stats::medpolish()];
##'
##' - [robustSummary()] to calculate a robust aggregation using
##'   [MASS::rlm()];
##'
##' - [base::colMeans()][base::colSums()] to use the mean of each
##' column;
##'
##' - [base::colSums()] to use the sum of each column;
##'
##' - [matrixStats::colMedians()][matrixStats::rowMedians()] to use
##'   the median of each column.
##'
##' @section Matrix-based aggregation functions:
##'
##' When aggregating with an adjacency matrix, user-defined
##' functions must return a new matrix. Examples thereof are:
##'
##' - `colSumsMat(x, MAT)` aggregates by the summing the peptide intensities
##'    for each protein. Shared peptides are re-used multiple times.
##'
##' - `colMeansMat(x, MAT)` aggregation by the calculating the mean of
##'    peptide intensities. Shared peptides are re-used multiple
##'    times.
##'
##' @family Quantitative feature aggregation
##'
##' @name aggregate
##'
##' @aliases aggregate_by_vector aggregate_by_vector
##'
##' @author Laurent Gatto
##'
##' @examples
##'
##' x <- matrix(c(10.39, 17.16, 14.10, 12.85, 10.63, 7.52, 3.91,
##'               11.13, 16.53, 14.17, 11.94, 11.51, 7.69, 3.97,
##'               11.93, 15.37, 14.24, 11.21, 12.29, 9.00, 3.83,
##'               12.90, 14.37, 14.16, 10.12, 13.33, 9.75, 3.81),
##'             nrow = 7,
##'             dimnames = list(paste0("Pep", 1:7), paste0("Sample", 1:4)))
##' x
##'
##' ## -------------------------
##' ## Aggregation by vector
##' ## -------------------------
##'
##' (k <- paste0("Prot", c("B", "E", "X", "E", "B", "B", "E")))
##'
##' aggregate_by_vector(x, k, colMeans)
##' aggregate_by_vector(x, k, robustSummary)
##' aggregate_by_vector(x, k, medianPolish)
##'
##' ## -------------------------
##' ## Aggregation by matrix
##' ## -------------------------
##'
##' adj <- matrix(c(1, 0, 0, 1, 1, 1, 0, 0,
##'                 1, 0, 1, 0, 0, 1, 0, 0,
##'                 1, 0, 0, 0, 1),
##'               nrow = 7,
##'               dimnames = list(paste0("Pep", 1:7),
##'                               paste0("Prot", c("B", "E", "X"))))
##' adj
##'
##' ## Peptide 4 is shared by 2 proteins (has a rowSums of 2),
##' ## namely proteins B and E
##' rowSums(adj)
##'
##' aggregate_by_matrix(x, adj, colSumsMat)
##' aggregate_by_matrix(x, adj, colMeansMat)
##'
##' ## ---------------
##' ## Missing values
##' ## ---------------
##'
##' x <- matrix(c(NA, 2:6), ncol = 2,
##'             dimnames = list(paste0("Pep", 1:3),
##'                             c("S1", "S2")))
##' x
##'
##' ## simply use na.rm = TRUE to ignore missng values
##' ## during the aggregation
##'
##' (k <- LETTERS[c(1, 1, 2)])
##' aggregate_by_vector(x, k, colSums)
##' aggregate_by_vector(x, k, colSums, na.rm = TRUE)
##'
##'
##' ## NAs are propagated during the matrix
##' ## multiplication in the aggregation functon
##' (adj <- matrix(c(1, 1, 0, 0, 0, 1), ncol = 2,
##'                dimnames = list(paste0("Pep", 1:3),
##'                                c("A", "B"))))
##'
##' aggregate_by_matrix(x, adj, colSumsMat)
##' ## not implemented
##' try(aggregate_by_matrix(x, adj, colSumsMat, na.rm = TRUE))
##' colSumsMat
NULL
