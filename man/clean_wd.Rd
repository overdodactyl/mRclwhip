% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_wd.R
\name{clean_wd}
\alias{clean_wd}
\title{Move all html, pdf, and docx files to an output directory}
\usage{
clean_wd(
  input_dir = ".",
  output_dir = "./documents/",
  file_ext = c("docx", "html", "pdf")
)
}
\arguments{
\item{input_dir}{Path of directory to clean}

\item{output_dir}{Path to output directory}

\item{file_ext}{Extension of file to move}
}
\description{
Move all html, pdf, and docx files to an output directory
}
