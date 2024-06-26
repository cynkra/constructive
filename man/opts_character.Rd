% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-character.R
\name{opts_character}
\alias{opts_character}
\title{Constructive options for type 'character'}
\usage{
opts_character(
  constructor = c("default"),
  ...,
  trim = NULL,
  fill = c("default", "rlang", "+", "...", "none"),
  compress = TRUE,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE
)
}
\arguments{
\item{constructor}{String. Method used to construct the object, often the name
of a function.}

\item{...}{Constructive options built with the \verb{opts_*()} family of functions. See the "Constructive options"
section below.}

\item{trim}{\code{NULL} or integerish. Maximum of elements showed before it's trimmed.
Note that it will necessarily produce code that doesn't reproduce the input.
This code will parse without failure but its evaluation might fail.}

\item{fill}{String. Method to use to represent the trimmed elements. See \code{?opts_atomic}}

\item{compress}{Boolean. If \code{TRUE} instead of \code{c()} Use \code{seq()}, \code{rep()}
when relevant to simplify the output.}

\item{unicode_representation}{By default "ascii", which means only ASCII characters
(code point < 128) will be used to construct strings and variable names. This makes sure that
homoglyphs (different spaces and other identically displayed unicode characters)
are printed differently, and avoid possible unfortunate copy and paste
auto conversion issues. "latin" is more lax and uses all latin characters
(code point < 256). "character" shows all characters, but not emojis. Finally
"unicode" displays all characters and emojis, which is what \code{dput()} does.}

\item{escape}{Boolean. Whether to escape double quotes and backslashes. If \code{FALSE} we use
single quotes to surround strings (including variable and element names)
containing double quotes, and raw strings for strings that contain backslashes
and/or a combination of single and double quotes. Depending on
\code{unicode_representation} \code{escape = FALSE} cannot be applied on all strings.}
}
\value{
An object of class <constructive_options/constructive_options_character>
}
\description{
These options will be used on objects of type 'character'. This type has
a single native constructor, but some additional options can be set.

\code{unicode_representation} and \code{escape} are usually better set in the main
function (\code{construct()} or other) so they apply not only on strings but on
symbols and argument names as well.

To set options on all atomic types at once see \link{opts_atomic}().
}
