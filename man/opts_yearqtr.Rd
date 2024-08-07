% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-yearqtr.R
\name{opts_yearqtr}
\alias{opts_yearqtr}
\title{Constructive options for class 'yearqtr'}
\usage{
opts_yearqtr(constructor = c("as.yearqtr", "yearqtr", "next"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_yearqtr>
}
\description{
These options will be used on objects of class 'yearqtr'.
}
\details{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"as.yearqtr"} (default): We build the object using \code{zoo::as.yearqtr()} on
a string in the format \code{"2000 Q3"}.
\item \code{"yearqtr"} : We build the object using \code{zoo::yearqtr()} on a double in the
format \code{2000.5}
\item \code{"next"} : Use the constructor for the next supported class.
}
}
