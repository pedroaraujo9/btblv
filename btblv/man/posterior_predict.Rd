% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_predict.R
\name{posterior_predict}
\alias{posterior_predict}
\title{Posterior predictive sample and summary for the btblv model}
\usage{
posterior_predict(btblv_posterior, seed, cred_mass = 0.95)
}
\arguments{
\item{btblv_posterior}{\code{btblv_posterior} object generated by \code{extract_posterior}.}

\item{seed}{int seed for the random values.}

\item{cred_mass}{numeric value in the interval (0, 1) with the mass of the HPD interval.}
}
\value{
A list with the elements:
\itemize{
\item \code{pred_post_summary_df} - \code{data.frame} with the group, time, item,
and summary statistics for the observation.
\item \code{pred_post_sample} - matrix (iteration x observations) with the posterior predictive sample.
}
}
\description{
Posterior predictive sample and summary for the btblv model
}
\examples{
# example_fit$single_K1 \%>\% extract_posterior() \%>\% posterior_predict()
}
