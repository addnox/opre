% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper-string.R
\name{parse_date}
\alias{parse_date}
\title{Parse a character vector to Date}
\usage{
parse_date(x)
}
\description{
Parse a character vector to Date
}
\examples{
x <- c("2021-01-03", "12-31-2019", "Jan 2 2022", "2023/1/1")
parse_date(x)
}
