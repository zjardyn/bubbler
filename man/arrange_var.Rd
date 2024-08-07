% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrange.R
\name{arrange_var}
\alias{arrange_var}
\title{Arrange a variable by the ordering of another variable}
\usage{
arrange_var(rel_abund_tb, var = "sample_id", levels)
}
\arguments{
\item{rel_abund_tb}{A relative abundance table in tibble form.}

\item{var}{The variable to sort.}

\item{levels}{The variable to sort with.}
}
\value{
A tibble with a sorted variable.
}
\description{
Arrange a variable by the ordering of another variable
}
\examples{
rel_abund_phy(phy = physeq, meta_data = TRUE) \%>\%
    arrange_var(levels = "location")
}
