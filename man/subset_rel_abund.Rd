% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.R
\name{subset_rel_abund}
\alias{subset_rel_abund}
\title{Subset samples from a rel_abund tibble and recompute rel_abund}
\usage{
subset_rel_abund(rel_abund_tb, var, selection)
}
\arguments{
\item{var}{A character vector of the variable to subset from.}

\item{selection}{A character vector of elements to select.}

\item{rel_abund_tab}{A tibble.}
}
\value{
A tibble.
}
\description{
Subset samples from a rel_abund tibble and recompute rel_abund
}
\examples{
smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
subset_rel_abund(rel_abund_tab, var = "sample_id", selection = smp_selection)
}
