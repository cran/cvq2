\name{cvq2}
\alias{cvq2}
%- Also NEED an '\alias' for EACH other topic documented here.
%do not use \eqn in title
\title{
  Calculation of a model's predictive power without external data set. 
  A cross validation is applied to the data set, which is the base of a model. 
}
\description{
  %which is used to construct the model. 
  A cross validation is performed to the data set, that represents the model. 
  The cross validation splits the data set (\eqn{N}{N} elements) into a training set (\eqn{N-k}{N-k} elements) and a test set (\eqn{k}{k} elements). 
  The training set is used to predict the missing \eqn{k}{k} value(s).
  Any observed value is predicted once within one iteration.
  If \eqn{k > 1}{k>1} one can repeat this calculation with different distributions of training and test sets to overcome biasing.
  The comparison between predicted and observed values yields to the predictive squared correlation coefficient, \eqn{q^2_{cv}}{q^2_cv}.
}
\usage{
  cvq2( data, formula = NULL, nGroup = N, nRun = 1,
  round = 4, extOut = FALSE, extOutFile = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ 
    The data set consists of parameter \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observed value \emph{y}
  }
  \item{formula}{
    The formula used to predict the observed value, like \eqn{y} ~ \eqn{x_1 + x_2 + \ldots + x_n}{y ~ x_1 + x_2 + ... + x_n}\cr
    \strong{DEFAULT: NULL}\cr
    If NULL, a generic formula is derived from the data set, assuming that the last column contains the observed value
  }
  \item{nGroup}{
    Number of individual test sets generated from the data set, \strong{DEFAULT: \emph{N}}, \eqn{1 <= nGroup <= N}{1 <= nGroup <= N}
  }
  \item{nRun}{
    Number of iterations, the data set is split into individual training and test sets.
    This corresponds to the number of individual predictions per observed value, \strong{DEFAULT: 1}
    %correlates with nGroup -> nGroup == N, than nRun can be 1 only
    Must be \eqn{1}{1}, if \eqn{nGroup = N}{nGroup = N}.   
  }
  \item{round}{
  The rounding value, \strong{DEFAULT: 4}
}
  \item{extOut}{
  Extended output, \strong{DEFAULT: FALSE}\cr
  If \code{extOutFile} is not specified, write to stdout()
}
  \item{extOutFile}{
  Write extended output into file (\emph{implies} \code{extOut = TRUE}), \strong{DEFAULT: NULL}
}
}
\details{
The \code{cvq2()}-method performs a cross validation for a data set with \code{nGroup} training and test sets.\cr
The given data set is split into several groups, whereas one group will be the test set and the others are merged together as training set.
Each group consist of \eqn{k}{k} elements:
\deqn{k = \left\lceil\frac{N}{nGroup}\right\rceil}{k=ceil(N/nGroup)}
In general, each test set has size \eqn{k}{k}, whereas the training set has size \eqn{N-k}{N-k}. 
In case, \eqn{\frac{N}{nGroup}}{k=N/nGroup} is a decimal number, some groups consist of \eqn{k-1}{k-1} elements.
 
For each test set, the training set with the remaining values is used to construct a model to predict the observed values from the test set.
This model is slighlty different compared to the model for the \eqn{r^2}{r^2} calculation, which is due to the missing \emph{k} values.
The difference between the prediction and the observation is used to calculate the \strong{PRE}dictive residual \strong{S}um of \strong{S}quares (\emph{PRESS}).
Furthermore for any training set, the mean of the observed values, \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, is calculated.
With \emph{PRESS} and \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, the modified \eqn{q^2_{cv}}{q^2_cv} equation is used to calculate the predictive squared correlation coefficient.

Additionally, the conventional squared correlation coefficient, \eqn{r^2}{r^2}, is calculated with a linear regression for the entire data set.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%names(result$cv)
%% \link[dest=cvq2-class]{cvq2} OR \linkS4class{cvq2}
  The object of class \code{"\linkS4class{cvq2}"} contains results of linear regression and cross validation.
  Thereby one get the information about the model calibration and prediction performance described by the data set. 
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Torsten Thalheim <torstenthalheim@gmx.de>
}
\concept{cross validation}
\concept{Pearson correlation coefficient}
\concept{squared correlation coefficient}
%%\note{
%%  ~~further notes~~
%%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%%\seealso{}
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
\examples{
library(cvq2)
data(cvq2.setA)
result <- cvq2( cvq2.setA, y ~ x1 + x2 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nGroup = 3 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nGroup = 3, nRun = 5 )
result

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% __ONLY ONE__ keyword per line
\keyword{ q^2 }
\keyword{ q square }
\keyword{ predictive squared correlation coefficient }
\keyword{ cross valdiation }
