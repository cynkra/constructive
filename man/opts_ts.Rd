% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-ts.R
\name{opts_ts}
\alias{opts_ts}
\title{Constructive options for time-series objets}
\usage{
opts_ts(constructor = c("ts", "next", "atomic"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_ts>
}
\description{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"ts"} : We use \code{ts()}
\item \code{"next"} : Use the constructor for the next supported class. Call \code{.class2()}
on the object to see in which order the methods will be tried. This will usually
be equivalent to \code{"atomic"}
\item \code{"atomic"} : We define as an atomic vector and repair attributes
}
}
