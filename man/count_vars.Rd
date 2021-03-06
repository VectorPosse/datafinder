% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_vars.R
\name{count_vars}
\alias{count_vars}
\title{Count the number of variables (by class) in data frames}
\usage{
count_vars(dataframe)
}
\arguments{
\item{dataframe}{One or more data frames (as a character string or
a vector of character strings).}
}
\value{
A data frame. Each row gives the counts (by class) of all the
variables in each data frame in the dataframe argument. If the input
dataframe is not actually a data frame, then an empty data frame
will be returned.
}
\description{
The count_vars function will count the number of the variables (by class) in
a data frame.
}
\examples{
  count_vars("CO2")
  count_vars(c("CO2", "airquality"))

}
\author{
Sean Raleigh, \email{sraleigh@westminstercollege.edu}

Jack Wilburn \email{jackwilburn@tutanota.com}
}
