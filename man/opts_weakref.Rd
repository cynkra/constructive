% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-weakref.R
\name{opts_weakref}
\alias{opts_weakref}
\title{Constructive options for the class \code{weakref}}
\usage{
opts_weakref(constructor = c("new_weakref"), ...)
}
\arguments{
\item{constructor}{String. Name of the constructor.}

\item{...}{Additional options used by user defined constructors through the \code{opts} object}
}
\value{
An object of class <constructive_options/constructive_options_array>
}
\description{
These options will be used on objects of type \code{weakref}. \code{weakref} objects
are rarely encountered and there is no base R function to create them. However
\pkg{rlang} has a \code{new_weakref} function that we can use.
}
