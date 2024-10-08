% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation_score.R
\name{correlation_score}
\alias{correlation_score}
\title{Calculate Correlation-Weighted Composite Scores}
\usage{
correlation_score(
  data = .,
  composite_list,
  digits = 3,
  return_metrics = FALSE,
  file = NULL,
  name = NULL
)
}
\arguments{
\item{data}{A dataframe object. This should be a structured dataset where
each column represents a variable and each row represents an observation.}

\item{composite_list}{A required \code{composite_list} object. Each name in
the list represents a composite variable, and the corresponding vector
contains the column names that are associated with the indicators
comprising said composite variable.}

\item{digits}{The decimal places for the metrics to be rounded to. Default is
3. This argument is only relevant if \code{return_metrics = TRUE}.}

\item{return_metrics}{Logic to determine whether to return reliability and
validity metrics. Set to \code{TRUE} for a list of dataframes with
reliability and validity metrics.}

\item{file}{An optional file path. If specified, the results will be written
as a formatted excel workbook. This argument is only relevant if
\code{return_metrics = TRUE}.}

\item{name}{A required string denoting the name of the composite variable.}
}
\value{
If \code{return_metrics = FALSE}, a dataframe identical to the input
  dataframe, with additional columns appended at the end, is returned. These
  new columns represent the calculated composite scores. If
  \code{return_metrics = TRUE}, a list containing the following dataframes is
  returned:
 \itemize{
 \item \strong{Data}: A dataframe with the composite variables appended as new
 variables.
 \item \strong{Metrics}: A matrix of indicator loadings and weights metrics.
 \item \strong{Validity}: A matrix of composite reliability and validity
 metrics.
}
}
\description{
Create composite scores of scales by specifying the indicators
  that go into each respective composite variable.
}
\details{
Composite scores are calculated as the correlation-weighted mean of
  the indicators.

  Each indicator's correlation weight (\eqn{w}) is calculated as its average
  correlation with all other indicators. A correlation matrix is first
  computed using all indicators of the composite variable to obtain all
  possible combinations of pairwise bivariate correlations, such that a
  correlation matrix is calculated where the entry in the i-th row and j-th
  column is the correlation between the i-th and j-th indicators. The
  diagonal of the correlation matrix (i.e., correlation of i-th indicator to
  itself) are set to \code{NA} to remove self-correlations. Indicator weight
  calculation is then mathematically represented as:

\deqn{w[j] = \frac{1}{n} \sum_{i=1}^{n} cor\_matrix[i, j]}{w[j] = 1/n *
sum(cor_matrix[i, j] for i=1 to n)}

  where \eqn{n} is the number of rows in \eqn{cor\_matrix}, and the sum is
  taken over all \eqn{i} such that \eqn{cor\_matrix[i, j]} is not \code{NA}.
  The correlation weights are then normalized by dividing each weight by the
  mean of the weights:

\deqn{w[j] = \frac{w[j]}{\frac{1}{m} \sum_{k=1}^{m} w[k]}}{w[j] = w[j] / (1/m
* sum(w[k] for k=1 to m))}

where \eqn{m} is the number of columns in \eqn{cor\_matrix}, and the sum is
taken over all \eqn{k}. The correlation-weighted composite score is then
calculated as follows:

\deqn{\bar{cs}_c = \frac{1}{n} \sum_{i=1}^{n} I[i] * w_i}{composite_score_c = 1/n
* sum(df[i] * w_i for i=1 to n)}

where \eqn{\bar{cs}_c} is the correlation weighted composite score and
\eqn{I} is the raw indicator input. If \code{return_metrics} is set to
\code{TRUE}, the function also returns composite reliability and
validity metrics, as well as indicator-level loadings and weights. See the
documentation \code{?calc_metrics} for the calculation and reporting of
reliability and validity measures.

If \code{file} is specified with a file path, this function will
automatically write a formatted excel workbook of the returned output. The
format is consistent with what is typically reported in APA 7th edition, and
can be copy and pasted directly into a Word document or similar document
software.
}
\examples{

data(grit)

# Specify the named list with composite names and their respective indicators
composite_list <- composite_list(

  # Lower-order composites
  extraversion          = sprintf("e\%01d", seq(10)),
  neuroticism           = sprintf("n\%01d", seq(10)),
  agreeableness         = sprintf("a\%01d", seq(10)),
  conscientiousness     = sprintf("c\%01d", seq(10)),
  openness              = sprintf("o\%01d", seq(10)),
  consistency_interest  = sprintf("gs\%01d", c(2,3,5,7,8,11)),
  perseverance_effort   = sprintf("gs\%01d", c(1,4,6,9,10,12)),

  # Higher-order composites
  grit                  = c("consistency_interest", "perseverance_effort")

 )

# Calculate correlation-weighted composite scores
correlation_score(data = grit,
                  composite_list = composite_list)

# Calculate correlation-weighted composite scores, reliability, & validity
correlation_score(data = grit,
                  composite_list = composite_list,
                  digits = 3,
                  return_metrics = TRUE,
                  file = "composite.xlsx")

unlink("composite.xlsx")

}
