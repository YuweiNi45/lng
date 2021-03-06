% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smruni.R
\name{smruni}
\alias{smruni}
\title{Univariate analysis and summary table}
\usage{
smruni(y, data, alternative = c("linear", "logistic"), digit = 3, ...)
}
\arguments{
\item{y}{dependent variable which selected by user}

\item{data}{a data frame, list the variables to be summarized (contain the variable Y)}

\item{alternative}{a character string specifying the alternative approaches, must be one of "linear" (default), "logistic"}

\item{...}{further arguments to be passed to or from methods}

\item{digits}{the digits of the data values (default is 3)}
}
\value{
For univariate analysis, it returns a summary table based on the type of dependent variables. For continuous Y, the table contains parameters, coefficient, p-value, CI for coefficient; while for categorical variable, the table contains the parameters, Odds ratio, p-value, CI for odds ratio.
}
\description{
Generate summary table proving the univariate analysis for each independent variabls and dependent variable from the whole dataset
}
\references{
Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.
}
