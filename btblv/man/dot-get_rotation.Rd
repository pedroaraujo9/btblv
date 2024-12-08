% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.get_rotation}
\alias{.get_rotation}
\title{Compute the Procrustean matching rotation for each iteration of the posterior}
\usage{
.get_rotation(post_sample, reference_matrix)
}
\arguments{
\item{post_sample}{array with iters, param, latent dimension.}

\item{reference_matrix}{numeric matrix with reference to match.}
}
\value{
list with the Procrustean transform for each iteration.
}
\description{
Compute the Procrustean matching rotation for each iteration of the posterior
}
\examples{
#
}
