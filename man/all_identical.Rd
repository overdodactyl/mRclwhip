% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_identical.R
\name{all_identical}
\alias{all_identical}
\alias{any_identical}
\title{Check if any number of values are identical}
\usage{
all_identical(...)

any_identical(...)
}
\arguments{
\item{...}{Two or more values to compare}
}
\value{
Logical
}
\description{
Returns TRUE if all, or any, arguments are equal to each other using the test base::identical
}
\examples{
all_identical(1, 1, 1)
all_identical(1, 1, "1")
all_identical(1, 1, 2)
all_identical(1, 1, NA)
any_identical(1, 1, 2)
any_identical(1, "1", 2)
}
