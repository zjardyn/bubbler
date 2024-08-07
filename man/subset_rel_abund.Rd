% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.R
\name{subset_rel_abund}
\alias{subset_rel_abund}
\title{Subset samples from a relative abundance table.}
\usage{
subset_rel_abund(rel_abund_tb, var = "sample_id", selection, recompute = FALSE)
}
\arguments{
\item{rel_abund_tb}{A tibble.}

\item{var}{The variable to subset from.}

\item{selection}{The elements to select.}

\item{recompute}{Logical, whether to recompute rel_abund.}
}
\value{
A tibble.
}
\description{
Subset samples from a relative abundance table.
}
\examples{
smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
subset_rel_abund(rel_abund_phy(physeq), var = "sample_id", selection = smp_selection)
}
