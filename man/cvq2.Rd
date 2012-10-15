\name{cvq2}
\alias{cvq2}
%- Also NEED an '\alias' for EACH other topic documented here.
%do not use \eqn in title
\title{
Cross validation for a given data set and calculation of its predictive squared correlation coefficient.
}
\description{
Performs a cross validation for a given data set.
The cross validation split the data set (\eqn{N}{N} elements) into a training set (\eqn{N-k}{N-k} elements) and a test set (\eqn{k}{k} elements). 
The training set is used to predict the missing \eqn{k}{k} value(s).
Any observed value is predicted once and therewith, the predictive squared correlation coefficient \eqn{q^2_{cv}}{q^2_cv} is derived.
%later: could be predicted several times ???
}
\usage{
cvq2( data, formula, round = 4, extOut = FALSE, extOutFile = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ 
  The data set used for cross validation, consists of parameter \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observed value \emph{y}
}
  \item{formula}{
  The formula used to predict the observed value, like \eqn{y} ~ \eqn{x_1 + x_2 + \ldots + x_n}{y ~ x_1 + x_2 + ... + x_n}\cr
  \strong{DEFAULT: NULL}\cr
  If no formula is specified, a generic formula is derived from the data set, assuming that the last column contains the observed value
}
  \item{round}{
  The rounding value, \strong{DEFAULT: 4}
}
  \item{extOut}{
  extended output, \strong{DEFAULT: FALSE}\cr
  If \code{extOutFile} is not specified, write to stdout()
}
  \item{extOutFile}{
  write extended output into file (\emph{implies} \code{extOut = TRUE}), \strong{DEFAULT: NULL}
}
}
\details{
Currently this method performs a Leave-One-Out (LOO) cross validation (\eqn{k=1}{k=1}) only for a given data set.
First, the conventional squared correlation coefficient, \eqn{r^2}{r^2}, is calculated with a linear regression for the entire data set.
Afterwards, the given data set (\eqn{N}{N} elements) is split into a training set (\eqn{N-k}{N-k} elements) and a test set (\eqn{k}{k} elements). 
Then the predictive squared correlation coefficient, \eqn{q^2}{q^2}, can be calculated too.
For each test set, the training set with the remaining values is used to construct a model to predict the observed values from the test set.
This model is slighlty different compared to the model for the \eqn{r^2}{r^2} calculation, which is due to the missing \emph{k} values.
The difference between the predcition and the observation is used to calculate the \strong{PRE}dictive residual \strong{S}um of \strong{S}quares (\emph{PRESS}).
Furthermore for any training set, the mean of the observed values is calculated, annotated as \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}.
With \emph{PRESS} and \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, the modified \eqn{q^2_{cv}}{q^2_cv} equation is used to calculate the predictice squared correlation coefficient.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  result$cv - The cross validation results
  \item{elements_training_set}{The number of elements in the training set.}
  \item{elements_test_set}{The number of elements in the test set.}
  \item{datatable}{For each value, it contains the model parameters, the aritemtic mean of the training set, the observed and the predicted value.}
  \item{column_names}{The explanation of the datatable's column names.}
  \item{split_size_data_set}{The number of elements to split the data set.}
  \item{splitted_sets_count}{The amount of individual sets, the data set is splitted.}
%  \item{repeated_runs}{The number of repeated runs.}
  \item{q2$value }{The cross validation \eqn{q^2}{q^2} value.}
  \item{q2$rms}{The cross validation root mean square error.}

%  result$q2 - The predictive squared correlation coefficient
%  \item{value}{The \eqn{q^2}{q^2} value.}
%  \item{rms}{The root mean square error for the predictive squared correlation coefficient.}
  
  result$r2 - The conventional squared correlation coefficient
  \item{value}{The \eqn{r^2}{r^2} value.}
  \item{elements}{The number of elements in the data set.}
  \item{datatable}{Contains the observed and predicted values.}
  \item{column_names}{The explanation of the datatable's column names.}
  \item{observed_mean}{The arithemtic mean of the observed values.}
  \item{rms}{The linear regression root mean square error.}
%% ...
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Torsten Thalheim <torstenthalheim@gmx.de>
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{}
\examples{
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

library(cvq2)
data(cvq2.setA)
result <- cvq2( cvq2.setA, y ~ x1 + x2 )
result

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% __ONLY ONE__ keyword per line
\keyword{ q^2 }
\keyword{ q square }
\keyword{ predictive squared correlation coefficient }
\keyword{ cross valdiation }
