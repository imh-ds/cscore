% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_metrics.R
\name{export_metrics}
\alias{export_metrics}
\title{Export Metrics (Reliability & Validity)}
\usage{
export_metrics(metrics, digits, name = NULL, file)
}
\arguments{
\item{metrics}{A required list object. The list should contain an array for
composite scores under \code{data}, a data frame of indicator loadings and
weights under \code{metrics}, and composite validity value under
\code{validity}.}

\item{digits}{The decimal places for the metrics to be rounded to. Default is
3.}

\item{name}{A required string denoting the study/analysis name.}

\item{file}{A required file path. If specified, the results will be written
as a formatted excel workbook.}
}
\value{
A formatted excel workbook saved to specified file path.
}
\description{
A function to export a \code{metrics} object as a formatted
  excel workbook to the specified \code{file} path. The format is consistent
  with what is typically reported in APA 7th edition, and can be copy and
  pasted directly into a Word document or similar document software.
}
