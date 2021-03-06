% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data.R
\name{cleanData}
\alias{cleanData}
\title{Clean Data}
\usage{
cleanData(crime_data)
}
\arguments{
\item{crime_data}{a data frame to be cleaned}
}
\value{
A cleaned dataset with \code{NA} entries removed
}
\description{
Clean Data
}
\examples{
\dontrun{
data(sub_all_crime)
sub_all_crime = cleanData(sub_all_crime) }
}
