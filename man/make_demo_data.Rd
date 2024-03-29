% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_demo_data.R
\name{make_demo_data}
\alias{make_demo_data}
\title{A function wrapper that serially executes all steps related to demographic
cleaning of raw XLSX membership data.}
\usage{
make_demo_data(df, ...)
}
\arguments{
\item{df}{a tibble of demographic data in raw format}

\item{...}{}
}
\value{
a cleaned tibble with a subset of relevant features
}
\description{
A function wrapper that serially executes all steps related to demographic
cleaning of raw XLSX membership data.
}
\examples{
#make_demo_data(df = demo_raw)
}
