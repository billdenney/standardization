% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_match_colnames.R
\name{meta_match_colnames}
\alias{meta_match_colnames}
\title{Matches for variables based on metadata labels, and selects applicable variables present in source dataset}
\usage{
meta_match_colnames(data, file, coltype)
}
\arguments{
\item{data}{a data.frame that serves as source dataset}

\item{file}{specifications file(list or .xlsx) that informs selection of variables}

\item{coltype}{is column type specification for baseline, demog, measurement, or dosing variables}
}
\value{
a data frame with specified variables selected
}
\description{
matches for variables based on metadata labels, selects applicable variables present in source dataset, and creates empty columns for missing variables
}
\examples{
df<-data.frame(STUDYID=rep("S-CDSK-01",3),DOMAIN=rep("YEARS",3),Age=c(72,66,78))
list1<-data.frame(`Column Name`="AGE",
`Column Name Variants`="Age",
Labels="Age in years at baseline",
`Column Type`="Demographics",
Format="Numeric")
labelled::var_label(df[,"Age"]) <- "Age in years at baseline"
specification<-list(list1)
names(specification)<-"Specification-Source Data"
format_df<-meta_match_colnames(df,file=specification,coltype="Demographics")
}
