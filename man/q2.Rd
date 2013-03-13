\name{q2}
\alias{q2}
\alias{qsq}
\alias{qsquare}
\alias{cvq2}
\alias{cvqsq}
\alias{cvqsquare}
\alias{looq2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Model prediction power calculation.
}
\description{
  Determines the prediction power of a model.
  Therefore the model is applied to an external data set, and its observations are compared to the model predictions.
  If an external data set is not available, the prediction power is calculated while performing a cross-validation to the model data set.
}
\usage{
  looq2( modelData, formula = NULL, round = 4, extOut = FALSE, 
  extOutFile = NULL )

  cvq2( modelData, formula = NULL, nFold = N, nRun = 1, 
  round = 4, extOut = FALSE, extOutFile = NULL )

  q2( modelData, predictData, formula = NULL, round = 4, 
  extOut = FALSE, extOutFile = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelData}{ 
    The model data set consists of parameter \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observation \emph{y}
  }
  \item{predictData}{
    The prediction data set consists of parameter \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observation \emph{y}
}
  \item{formula}{
    The formula used to predict the observed value, like \eqn{y} ~ \eqn{x_1 + x_2 + \ldots + x_n}{y ~ x_1 + x_2 + ... + x_n}
    \strong{DEFAULT: NULL}\cr
    If NULL, a generic formula is derived from the data set, assuming that the last column contains the observed value
  }
  \item{nFold}{
    The model data set \code{modelData} is randomly partitioned into \var{n} equal sized subsets (test sets) during each run of cross-validation, \strong{DEFAULT: \emph{N}}, \eqn{2 <= nFold <= N}{2 <= nFold <= N}
  }
  \item{nRun}{
    Number of iterations, the cross-validation is applied to the data set.
    This corresponds to the number of individual predictions per observed value, \strong{DEFAULT: 1}, \eqn{1 <= nRun}{1 <= nRun}
    %correlates with nFold -> if nFold == N, than nRun can be 1 only
    Must be \eqn{1}{1}, if \eqn{nFold = N}{nFold = N}.   
  }
  \item{round}{
    The rounding value used in the output, \strong{DEFAULT: 4}
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
  The calibration of \code{modelData}, including the conventional squared correlation coefficient, \eqn{r^2}{r^2}, is calculated with a linear regression. 
  \subsection{\code{q2()-method}}{
    \cr
    \emph{Alias:} \code{qsq()}, \code{qsquare()}
    
    The model described by \code{modelData} is used to predict the observations of \code{predictData}.
%%    \link{link to this equation} 
    These predictions are used in the \eqn{q^2_{tr}}{q^2_tr} equation to calculate the predictive squared correlation coefficient.
  }
  \subsection{\code{cvq2()-method}}{
    \cr
    \emph{Alias:} \code{cvqsq()}, \code{cvqsquare()}
    
    A cross-validation is performed for \code{modelData}, whereas \code{modelData} (\eqn{N}{N} elements) is split into \code{nFold} disjunct and equal sized test sets (subsets).\cr
    Each test set consists of \eqn{k}{k} elements:
    \deqn{k = \left\lceil\frac{N}{nFold}\right\rceil}{k=ceil(N/nFold)}
    In case, \eqn{\frac{N}{nFold}}{k=N/nFold} is a decimal number, some test sets consist of \eqn{k-1}{k-1} elements.
    The remaining \eqn{N-k}{N-k} elements are merged together as training set for this test set and describe the model \var{M'}.
    This model is used to predict the observations in the test set.
    Note, that \var{M'} is slighlty different compared to the model \var{M} for the \eqn{r^2}{r^2}-calculation, which is a result of the missing \emph{k} values.\cr
    Each observation from \code{modelData} is predicted once.
    The difference between the prediction and the observation within the test sets is used to calculate the \strong{PRE}dictive residual \strong{S}um of \strong{S}quares (\emph{PRESS}).
    Furthermore for any training set, the mean of the observed values, \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, is calculated.
    With \emph{PRESS} and \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, the modified \eqn{q^2_{cv}}{q^2_cv} equation is used to calculate the predictive squared correlation coefficient.\cr
    
    In case \eqn{k > 1}{k>1} one can repeat the cross-validation to overcome biasing.
    Therefore, in each iteration (\eqn{\code{nRun} = 1 \ldots x}{nRun = 1 \ldots x}), the test sets are compiled individually by random.
    Within one iteration, each observation is predicted once.
    If \eqn{\code{nFold} = N}{nFold = N}, one need \emph{one} iteration only.
  %%    \link{link to this equation}
  }
  \subsection{\code{looq2()-method}}{
    \cr
    Same procedure as cvq2()-method (see above), but implicit \eqn{\code{nFold} = N}{nFold = N} to perform a Leave-One-Out cross-validation.
    For Leave-One-Out cross-validation one need \emph{one} iteration (\code{nRun = 1}) only.
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%names(result$cv)
%% \link[dest=cvq2-class]{cvq2} OR \linkS4class{cvq2}
  \subsection{\code{q2()-method}}{
    \cr
    The method \code{\link{q2}} returns an object of class \code{"\linkS4class{q2}"}.
    It contains information about the model calibration and its prediction performance on the external data set.
  }
  \subsection{\code{cvq2()-method, looq2()-method}}{
    \cr
    The methods \code{\link{cvq2}} and \code{\link{looq2}} return an object of class \code{"\linkS4class{cvq2}"}.
    It contains information about the model calibration and its prediction performance described by the model data set.
    Furthermore this object contains data about the cross-validation applied to the model data set.
  }
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
  Torsten Thalheim <torstenthalheim@gmx.de>
}
\concept{cross-validation}
\concept{Pearson correlation coefficient}
\concept{squared correlation coefficient}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
library(cvq2)
data(cvq2.setA)
result <- cvq2( cvq2.setA, y ~ x1 + x2 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nFold = 3 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nFold = 3, nRun = 5 )
result

data(cvq2.setA)
result <- looq2( cvq2.setA, y~x1+x2 )
result

data(cvq2.setA)
data(cvq2.setA_pred)
result <- q2( cvq2.setA, cvq2.setA, y~x1+x2 )
result 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ q^2 }
\keyword{ q square }
\keyword{ predictive squared correlation coefficient }
\keyword{ cross validation }
\keyword{ cross-validation }
\keyword{ prediction power }
