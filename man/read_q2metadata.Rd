% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qiime2R.R
\name{read_q2metadata}
\alias{read_q2metadata}
\title{read qiime2 metadata (.tsv)}
\usage{
read_q2metadata(file)
}
\arguments{
\item{file}{path to the input file, ex: file="~/data/moving_pictures/table.qza"}
}
\value{
a data.frame wherein the first column is SampleID
}
\description{
Loads a qiime2 metadata file wherein the 2nd line contains the #q2:types line dictating the type of variable (categorical/numeric)
}
\examples{
fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
read_q2metadata(fpath)
}
