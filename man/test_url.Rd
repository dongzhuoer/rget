% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{test_url}
\alias{test_url}
\title{test whether a url can be accessed}
\usage{
test_url(url)
}
\arguments{
\item{url}{string.}
}
\value{
logical scalar.
}
\description{
test whether a url can be accessed
}
\section{example}{


test_url('www.cookbook-r.com')
test_url('www.cookbook-r.com/Graphs2')  # not exist
test_url('www.cookbook-r.com/Data_input_and_output/Loading_data_from_a_file/datafile.csv')
test_url('yihui.name')
test_url('www.cosx.org')
test_url('www.baidu.com')
test_url('www.github.com')
test_url('www.google.com')  # of course fails and takes several seconds
}

