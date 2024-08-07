% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asv_rel_abund.R
\name{rel_abund_qiime}
\alias{rel_abund_qiime}
\title{Generate a relative abundance table in tibble form, from QIIME2 artifacts.}
\usage{
rel_abund_qiime(
  asv_qiime,
  taxa_qiime = NULL,
  metadata_qiime = NULL,
  taxa_level = "Phylum",
  var = NULL
)
}
\arguments{
\item{asv_qiime}{A count table in .qza format.}

\item{taxa_qiime}{A taxonomy table in .qza format.}

\item{metadata_qiime}{A metadata table in .tsv format. With second row as commented variable descriptions.}

\item{taxa_level}{The level to set the taxa, from Domain:Species.}

\item{var}{A variable to group by when computing relative abundance.}
}
\value{
A tibble.
}
\description{
Generate a relative abundance table in tibble form, from QIIME2 artifacts.
}
\examples{
asv_q <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
taxa_q <- system.file("extdata/qiime", "taxonomy.qza", package = "bubbler")
meta_q <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
rel_abund_qiime(asv_q, taxa_q, meta_q)
}
