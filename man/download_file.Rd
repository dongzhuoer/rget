% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{download_file}
\alias{download_file}
\title{using wget to download file}
\usage{
download_file(url, dest_file, force, verbose)
}
\arguments{
\item{url}{string.}

\item{dest_file}{string.}

\item{force}{logical scalar. Always download file, even when server file
isn't newer.}

\item{verbose}{logical scalar.}
}
\value{
logical scalar
}
\description{
using wget to download file only when server version is newer
}
