% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colourscheme.R
\name{add_unclassified}
\alias{add_unclassified}
\title{Add an "Unclassified" observation to a relative abundance table.}
\usage{
add_unclassified(rel_abund_tb)
}
\arguments{
\item{rel_abund_tb}{A relative abundance table in tibble format.}
}
\value{
A tibble.
}
\description{
Add an "Unclassified" observation to a relative abundance table.
}
\examples{
rel_abund_phy(physeq) \%>\% add_unclassified()
}
