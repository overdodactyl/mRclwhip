% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_tbl.R
\name{missing_tbl}
\alias{missing_tbl}
\title{Generate a table of "missingness"}
\usage{
missing_tbl(data, depends = NA)
}
\arguments{
\item{data}{A tbl.}

\item{depends}{A named vector representing variable dependencies.  For example, c("B" = "A", "C" = "B")
can be read as "B depends on A and C depends on B".  If a value for B is missing, it will only be counted if A is
not missing, and similarly for C and B.}
}
\description{
Create a table of missing values along with percentages.
It may be undesirable to count a variable if it is dependent on another one.
This can be accounted for here by using a named vector where the name is the dependent variable and the
value is the indpendent variable.  c("B" = "A") can be thought of here as "B depends on A".
In other words, each entry in B will only be evaluated if A is not missing.
If the input is a labeled data frame, the final output will include a column of labels.
}
\examples{
data <- tibble::tribble(
  ~A, ~B, ~C,
  1, 2, 3,
  NA, NA, 6,
  7, NA, 9
)

data

missing_tbl(data)

missing_tbl(data, depends = c("B" = "A"))

}
