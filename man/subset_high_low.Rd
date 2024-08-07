% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose.R
\name{subset_high_low}
\alias{subset_high_low}
\title{Subset the highest or lowest abundance samples from a relative abundance table.}
\usage{
subset_high_low(rel_abund_tb, subset = "low", n = 10, flip = FALSE)
}
\arguments{
\item{rel_abund_tb}{A relative abundance table in tibble form.}

\item{subset}{The subset choice, of "high" or "low"}

\item{n}{The number of samples.}

\item{flip}{Subset the opposite set of samples, logical.}
}
\value{
A tibble.
}
\description{
Subset the highest or lowest abundance samples from a relative abundance table.
}
\examples{
rel_abund_phy(phy = physeq) \%>\%
  subset_high_low(n = 5)
}
