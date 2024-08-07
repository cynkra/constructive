% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-zoo.R
\name{opts_zoo}
\alias{opts_zoo}
\title{Constructive options for class 'zoo'}
\usage{
opts_zoo(constructor = c("zoo", "next"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_zoo>
}
\description{
These options will be used on objects of class 'zoo'.
}
\details{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"zoo"} (default): We build the object using \code{zoo::zoo()}.
\item \code{"next"} : Use the constructor for the next supported class.
}
}
