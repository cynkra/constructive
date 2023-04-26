% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-POSIXlt.R
\name{opts_POSIXlt}
\alias{opts_POSIXlt}
\title{Constructive options for class 'POSIXlt'}
\usage{
opts_POSIXlt(
  constructor = c("as.POSIXlt", "next", "atomic"),
  ...,
  origin = "1970-01-01"
)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the environment, see Details section.}

\item{...}{Should not be used. Forces passing arguments by name.}

\item{origin}{Origin to be used, ignored when irrelevant.}
}
\value{
An object of class <constructive_options/constructive_options_factor>
}
\description{
These options will be used on objects of class 'POSIXlt'.
}
\details{
Depending on \code{constructor}, we construct the environment as follows:
\itemize{
\item \code{"as.POSIXlt"} (default): Build the object using a \code{as.POSIXlt()} call on a
character vector.
\item \code{"next"} : Use the constructor for the next supported class. Call \code{.class2()}
on the object to see in which order the methods will be tried.
\item \code{"atomic"} : We define as an atomic vector and repair attributes.
}
}