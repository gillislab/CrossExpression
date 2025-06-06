% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CrossExpression.R
\name{cross_expression_correlation}
\alias{cross_expression_correlation}
\title{Computes gene-gene correlations between cross-expressing cell-neighbor pairs.
Cell and neighbor masks are used to consider mutually exclusive expression
per gene pair.}
\usage{
cross_expression_correlation(
  data,
  locations,
  neighbor = 1,
  output_matrix = FALSE
)
}
\arguments{
\item{data}{A cells by genes expression matrix.}

\item{locations}{A cells by coordinates (x-y or higher dimensions) matrix.}

\item{neighbor}{The n-th nearest neighbor for computing correlations.}

\item{output_matrix}{If TRUE, outputs the cross-expression correlation
matrix.}
}
\value{
Returns a gene list with cross-expression correlation for each
gene pair.
}
\description{
Computes gene-gene correlations between cross-expressing cell-neighbor pairs.
Cell and neighbor masks are used to consider mutually exclusive expression
per gene pair.
}
\examples{
data(expression)
data(locations)
cross_expression_correlation(
 data = expression,
 locations = locations,
 neighbor = 1,
 output_matrix = FALSE
)
}
