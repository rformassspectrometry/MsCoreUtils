% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalizeMethods}
\alias{normalizeMethods}
\alias{normalize_matrix}
\title{Quantitative data normalisation}
\usage{
normalizeMethods()

normalize_matrix(x, method, ...)
}
\arguments{
\item{x}{A matrix or an \code{HDF5Matrix} object to be normalised.}

\item{method}{\code{character(1)} defining the normalisation
method. See \code{normalizeMethods()} for available ones.}

\item{...}{Additional parameters passed to the inner normalisation
function.}
}
\value{
A matrix of same class as \code{x} with dimensions \code{dim(x)}.
}
\description{
Function to normalise a matrix of quantitative omics data. The
nature of the normalisation is controlled by the \code{method}
argument, described below.
}
\details{
The \code{method} parameter can be one of \code{"sum"}, \code{"max"}, \code{"center.mean"},
\code{"center.median"}, \code{"div.mean"}, \code{"div.median"}, \code{"diff.median"},
\verb{"quantiles}", \verb{"quantiles.robust}" or \code{"vsn"}. The \code{normalizeMethods()}
function returns a vector of available normalisation methods.
\itemize{
\item For \code{"sum"} and \code{"max"}, each feature's intensity is divided by the
maximum or the sum of the feature respectively. These two methods are
applied along the features (rows).
\item \code{"center.mean"} and \code{"center.median"} center the respective sample
(column) intensities by subtracting the respective column means or
medians. \code{"div.mean"} and \code{"div.median"} divide by the column means or
medians.
\item \code{"diff.median"} centers all samples (columns) so that they all match the
grand median by subtracting the respective columns medians differences to
the grand median.
\item Using \code{"quantiles"} or \code{"quantiles.robust"} applies (robust) quantile
normalisation, as implemented in \code{\link[preprocessCore:normalize.quantiles]{preprocessCore::normalize.quantiles()}}
and \code{\link[preprocessCore:normalize.quantiles.robust]{preprocessCore::normalize.quantiles.robust()}}. \code{"vsn"} uses the
\code{\link[vsn:vsn2]{vsn::vsn2()}} function.  Note that the latter also glog-transforms the
intensities.  See respective manuals for more details and function
arguments.
}
}
\examples{
normalizeMethods()

## test data
set.seed(42)
m <- matrix(rlnorm(60), 10)

normalize_matrix(m, method = "sum")

normalize_matrix(m, method = "max")

normalize_matrix(m, method = "quantiles")

normalize_matrix(m, method = "center.mean")
}
\seealso{
The \code{\link[=scale]{scale()}} function that centers (like \code{center.mean} above) and
scales.
}
\author{
Laurent Gatto
}
