% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_pbold.R
\name{format_pbold}
\alias{format_pbold}
\title{Format a column}
\usage{
format_pbold(ft, pval, psig = 0.05, j = NA)
}
\arguments{
\item{ft}{A flextable object}

\item{pval}{(String) Name of pvalue colum}

\item{psig}{A numeric threshold to bold p-values}

\item{j}{A numeric column index.  If NA, it will be the last column in the table.}
}
\description{
Format a column of pvalues for a flextable
}
