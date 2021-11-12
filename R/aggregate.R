##' @title Aggreagate quantitative features
##'
##' @description
##'
##' These functions takes a matrix of quantitative features `x` and
##' aggregates the features (rows) according to either a factor
##' `INDEX` or an adjacency matrix `MAT`. The aggregation function is
##' defined by a user-defined function `FUN`.
##'
##' When aggregating by a vector/factor, the user-defined functions
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
##' When aggregating by an adjacency matrix, the user-defined
##' functions must return a new matrix. Examples thereof are:
##'
##'
##' - [colSumsMat()] aggregates by the summing the peptide intensities
##'    for each protein. Shared peptides are re-used multiple times.
##'
##' - [colMeansMat()] aggregation by the calculating the mean of
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
##' x <- structure(c(10.3961935744407, 17.1663715212693, 14.1027587989326,
##'                 12.850349037785, 10.6379251053134, 7.52885076885599,
##'                 3.91816118984218, 11.1339832690524, 16.5321471730746,
##'                 14.1787908569268, 11.9422579479634, 11.5154097311056,
##'                 7.69906817878979, 3.97092153807337, 11.9394664781386,
##'                 15.3791100898935, 14.2409281956285, 11.2106867261254,
##'                 12.2958526883634, 9.00858488668671, 3.83120129974963,
##'                 12.9033445520186, 14.375814954807, 14.1617803596661,
##'                 10.1237981632645, 13.3390344671153, 9.75719265786117,
##'                 3.81046169359919),
##'               .Dim = c(7L, 4L),
##'               .Dimnames = list(c("X1", "X27", "X41", "X47", "X52",
##'                                  "X53", "X55"),
##'                                c("iTRAQ4.114", "iTRAQ4.115",
##'                                  "iTRAQ4.116", "iTRAQ4.117")))
##' x
##'
##' ## -------------------------
##' ## Aggregation by vector
##' ## -------------------------
##'
##' k <- factor(c("B", "E", "X", "E", "B", "B", "E"))
##'
##' aggregate_by_vector(x, k, colMeans)
##' aggregate_by_vector(x, k, robustSummary)
##' aggregate_by_vector(x, k, medianPolish)
##'
##'
##' ## -------------------------
##' ## Aggregation by matrix
##' ## -------------------------
NULL
