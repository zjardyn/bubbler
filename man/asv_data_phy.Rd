% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_data.R
\name{asv_data_phy}
\alias{asv_data_phy}
\title{Grab asv table from a physeq object and format to tibble}
\usage{
asv_data_phy(phy)
}
\arguments{
\item{phy}{A phyloseq object containing tax_table}
}
\value{
A tibble.
}
\description{
Grab asv table from a physeq object and format to tibble
}
\examples{
asv_data_phy(physeq)
}
