% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_time_format.R
\name{date_time_format}
\alias{date_time_format}
\title{converts datetime variable to ISO 8601}
\usage{
date_time_format(data, col)
}
\arguments{
\item{data}{a data.frame with datetime variables}

\item{col}{is datetime variable to be converted}
}
\value{
datetime variable in ISO 8601 format with UN for missing date and time parts
}
\description{
converts datetime variable to ISO 8601
}
\examples{
dtc_df <- data.frame(ADTC = c(
"2004", "2015-04", "2015---04",
"2014-02-12", "2014-02-12T13:20", "2014-01-22T09:35:00",
"", NA
))
format_dtc <- date_time_format(dtc_df, "ADTC")
}
