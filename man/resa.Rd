% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{resa}
\alias{resa}
\title{RESA: The risk calculator for severe asthma exacerbations in patients with severe asthma}
\usage{
resa(data)
}
\arguments{
\item{data}{patient data matrix. Can have one or many patients in it. See \link{samplePatients}.}
}
\value{
patient data with predicted risk
}
\description{
RESA: The risk calculator for severe asthma exacerbations in patients with severe asthma
}
\examples{
results <- resa(data = samplePatients)
}
