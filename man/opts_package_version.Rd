% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-numeric_version.R
\name{opts_package_version}
\alias{opts_package_version}
\title{Constructive options for package_version}
\usage{
opts_package_version(constructor = c("package_version", "next", "list"), ...)
}
\arguments{
\item{constructor}{String. Name of the function used to construct the object.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_package_version>
}
\description{
Depending on \code{constructor}, we construct the object as follows:
\itemize{
\item \code{"package_version"} : We use \code{package_version()}
\item \code{"next"} : Use the constructor for the next supported class. Call \code{.class2()}
on the object to see in which order the methods will be tried. This will usually
be equivalent to \code{"array"}
\item \code{"list"} : We define as a list and repair attributes
}
}
