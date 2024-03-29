% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verify_ntsfd.R
\name{verify_ntsfd}
\alias{verify_ntsfd}
\title{Ensures there are no missing values in NTSFD column}
\usage{
verify_ntsfd(data, expected_missing)
}
\arguments{
\item{data}{Data.frame with STUDYID, USUBJID, VISIT, VISITDY, ATPTN, NTSFD variables.}

\item{expected_missing}{Specifies VISIT data for which missing values are expected in NTSFD (e.g. "Unscheduled").}
}
\value{
Data.frame with data rows where NTSFD is missing.
}
\description{
Verifies there are no missing values in NTSFD column.
}
\examples{
df1<-
  data.frame(
    STUDYID = rep("CDISK-01", 6),
    USUBJID = paste0("CDISC01.10000", c(1:6)),
    NTSFD=c(NA,"0","-7272","-384","0","0"),
    VISITDY=c(NA,"1","31","8","1","1"),
    VISIT=c("Unscheduled","1","Day 31","WEEK 1","Day 1","DAY 1"),
    ATPTN=c("3384","0","-7992","-552",	"0","0"),
    ATPT =c("141","0","-333","-23","Pre-Dose","0")
  )
verify_ntsfd(df1, expected_missing=c("Unscheduled"))

}
