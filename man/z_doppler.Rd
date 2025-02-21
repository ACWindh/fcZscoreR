% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_doppler.R
\name{z_doppler}
\alias{z_doppler}
\title{Calculation of z-scores for umbilical Doppler Sonography}
\usage{
z_doppler(index = "PI", value, GA, return_value = "z")
}
\arguments{
\item{index}{one of PI (pulsatility index), RI (resitance index), or ratio for ystolic/diastolic ratio}

\item{value}{the observed value for either PI, RI or ratio}

\item{GA}{estimated gestational age as exact weeks 20+1 = 20+1/7 = 20.1428571}

\item{return_value}{which parameter should be calculated can be "z" or "centile"}
}
\value{
z-score or percentile of the z-score
}
\description{
Calculation of z-scores for umbilical Doppler Sonography
}
\examples{
z_doppler(index="PI", value=1, GA=metricGA("36+4"), return_value = "z")
z_doppler(index="PI", value=1, GA=36+4/7, return_value = "centile")
}
\references{
Drukker et al. 2020 (INTERGROWTH-21st project), \url{https://pmc.ncbi.nlm.nih.gov/articles/PMC7287403/#sec2}
}
