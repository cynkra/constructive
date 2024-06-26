% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-externalptr.R
\name{opts_externalptr}
\alias{opts_externalptr}
\title{Constructive options for type 'externalptr'}
\usage{
opts_externalptr(constructor = c("default"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_externalptr>
}
\description{
These options will be used on objects of type 'externalptr'. By default this
function is useless as nothing can be set, this is provided in case users wan
to extend the method with other constructors.
}
\details{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"default"} : We use a special function from the constructive
}
}
