% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s7-S7_property.R
\name{opts_S7_property}
\alias{opts_S7_property}
\title{Constructive options for class 'S7_property'}
\usage{
opts_S7_property(constructor = c("new_property", "next"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_S7_property>
}
\description{
These options will be used on objects of class 'S7_property'.
}
\details{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"new_property"} (default): We build the object using \code{S7::new_property()}.
\item \code{"next"} : Use the constructor for the next supported class.
}
}
