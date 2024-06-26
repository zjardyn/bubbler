% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asv_rel_abund.R
\name{rel_abund_phy}
\alias{rel_abund_phy}
\title{Generate a relative abundance table in tibble format from a phyloseq object.}
\usage{
rel_abund_phy(phy, taxa_data, meta_data, taxa_level, var)
}
\arguments{
\item{phy}{A phyloseq object containing an otu_table and tax_table.}

\item{meta_data}{A logical value specifying if metadata should be included from phy.}

\item{taxa_level}{A character value specifying the taxa level from Domain to species.}

\item{var}{A character value of a variable to sum by.}
}
\value{
A tibble.
}
\description{
Generate a relative abundance table in tibble format from a phyloseq object.
}
\examples{
rel_abund_phy(phy = physeq1, taxa_level = "Phylum", var = NULL , meta_data = FALSE)
}
