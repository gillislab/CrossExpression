% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CrossExpression.R
\name{spatial_enrichment}
\alias{spatial_enrichment}
\title{Determines whether the supplied genes show spatial enrichment in
cross-expression.
Spatial enrichment can be interpreted as delineating anatomical boundaries.}
\usage{
spatial_enrichment(
  data,
  locations,
  gene1,
  gene2,
  neighbor = 1,
  max_pairs = 20000
)
}
\arguments{
\item{data}{A cells by genes expression matrix.}

\item{locations}{A cells by coordinates (x-y or higher dimensions) matrix.}

\item{gene1}{Name of gene1.}

\item{gene2}{Name of gene2.}

\item{neighbor}{The n-th nearest neighbor.}

\item{max_pairs}{Specify maximum number of cell pairs to consider. Lower
number increases computational efficiency.}
}
\value{
Returns a p-value and distance distributions between cross-expressing
cells and cross-expressing and random cells.
}
\description{
Determines whether the supplied genes show spatial enrichment in
cross-expression.
Spatial enrichment can be interpreted as delineating anatomical boundaries.
}
\examples{
data(expression)
data(locations)
expression_df <- as.data.frame(as.matrix(expression))
locations_df <- as.data.frame(as.matrix(locations))
enrichment <- spatial_enrichment(
 data = expression_df,
 locations = locations_df,
 gene1 = "Tafa1",
 gene2 = "Col19a1"
)
enrichment$pvalue
enrichment$target
enrichment$null
enrichment$plot
}
