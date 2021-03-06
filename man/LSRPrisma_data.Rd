% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSRPrisma_data.R
\name{LSRPrisma_data}
\alias{LSRPrisma_data}
\title{Read in Data for LSR Prisma Flow Diagram}
\usage{
LSRPrisma_data(data)
}
\arguments{
\item{data}{Input data frame based on 'inst/extdata/template.csv'.}
}
\value{
List of objects needed by 'LSRPRISMA_flow()'.
}
\description{
Function to read in data for 'LSRPrisma_flow()' from
the template file. The data are stored in columns, with each column
being an additional data point in a LSR incremental update.
}
\examples{
\dontrun{
data <- read.csv('inst/extdata/approach1.csv', stringsAsFactors=FALSE)
flowdata <- LSRPrisma_data(data)
}
}
