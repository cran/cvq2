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
  Determines the prediction power of model \var{M}.
  Therefore \var{M} is applied to an external data set and its observations are compared to the model predictions.
  If an external data set is not available, the prediction power is calculated while performing a cross-validation to the model data set.
}
\usage{
  looq2( modelData, formula = NULL, nu = 1, round = 4, 
  extOut = FALSE, extOutFile = NULL )

  cvq2( modelData, formula = NULL, nFold = N, nRun = 1, nu = 1, 
  round = 4, extOut = FALSE, extOutFile = NULL )

  q2( modelData, predictData, formula = NULL, nu = 0, round = 4, 
  extOut = FALSE, extOutFile = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelData}{ 
    The model data set consists of parameters \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observation \emph{y}
  }
  \item{predictData}{
    The prediction data set consists of parameters \eqn{x_1}{x_1}, \eqn{x_2}{x_2}, \ldots, \eqn{x_n}{x_n} and an observation \emph{y}
}
  \item{formula}{
    The formula used to predict the observation: \eqn{y \sim x_1 + x_2 + \ldots + x_n}{y ~ x_1 + x_2 + \ldots + x_n}
    \strong{DEFAULT: NULL}\cr
    If NULL, a generic formula is derived from the data set, assuming that the last column contains observations whereas the others contain parameters \eqn{x_1, x_2, \ldots, x_n}{x_1, x_2, \ldots, x_n}
  }
  \item{nFold}{
    The data set \code{modelData} is randomly partitioned into \var{nFold} equal sized subsets (test sets) during each run, \strong{DEFAULT: \emph{N}}, \eqn{2 <= nFold <= N}{2 <= nFold <= N}
  }
  \item{nRun}{
    Number of iterations, the cross-validation is repeated for this data set.
    This corresponds to the number of individual predictions per observation, \eqn{1 <= nRun}{1 <= nRun}, \strong{DEFAULT: 1}
    %correlates with nFold -> if nFold == N, than nRun can be 1 only
    Must be \eqn{1}{1}, if \eqn{nFold = N}{nFold = N}.   
  }
  \item{nu}{
    The degrees of freedom used in \var{rmse} calculation in relation to the prediction power, \strong{DEFAULT: 1} (\code{looq2(),cvq2()}), \strong{0} (else)
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
  The calibration of model \var{M} with \code{modelData} is done with a linear regression.
  \subsection{\code{q2()}}{
    \cr
    \emph{Alias:} \code{qsq()}, \code{qsquare()}
    
    The model described by \code{modelData} is used to predict the observations of \code{predictData}.
%%    \link{link to this equation} 
    These predictions are used to calculate the predictive squared correlation coefficient, \eqn{q^2}{q^2}.
  }
  \subsection{\code{cvq2()}}{
    \cr
    \emph{Alias:} \code{cvqsq()}, \code{cvqsquare()}
    
    A cross-validation is performed for \code{modelData}, whereas \code{modelData} (\var{N} elements) is split into \code{nFold} disjunct and equal sized test sets.
    Each test set consists of \var{k} elements:
    \deqn{k = \left\lceil\frac{N}{nFold}\right\rceil}{k=ceil(N/nFold)}
    In case \eqn{\frac{N}{nFold}}{k=N/nFold} is a decimal number, some test sets consist of \eqn{k-1}{k-1} elements.
    The remaining \eqn{N-k}{N-k} elements are merged together as training set for this test set and describe the model \var{M'}.
    This model is used to predict the observations in the test set.
    Note, that \var{M'} is slighlty different to model \var{M}, which is a result of the missing \emph{k} values.\cr                                            
    Each observation from \code{modelData} is predicted once.
    The difference between the prediction and the observation within the test sets is used to calculate the \strong{PRE}dictive residual \strong{S}um of \strong{S}quares (\emph{PRESS}).
    Furthermore for any training set the mean of the observed values in this training set, \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, is calculated.
    \emph{PRESS} and \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i} are required to calculate the predictive squared correlation coefficient, \eqn{q^2_{cv}}{q^2_cv}.\cr
    In case \eqn{k > 1}{k>1} one can repeat the cross-validation to overcome biasing.
    Therefore in each iteration (\eqn{\code{nRun} = \lbrace1, 2, \ldots, x\rbrace}{nRun = 1,2 \ldots, x}) the test sets are compiled individually by random.
    Within one iteration, each observation is predicted once.
    If \eqn{\code{nFold} = N}{nFold = N}, \emph{one} iteration is necessary only.
  %%    \link{link to this equation}
  }
  \subsection{\code{looq2()}}{
    \cr
    Same procedure as \code{cvq2()} (see above), but implicit \eqn{\code{nFold} = N}{nFold = N} to perform a Leave-One-Out cross-validation.
    For Leave-One-Out cross-validation \emph{one} iteration (\code{nRun = 1}) is necessary only.
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%names(result$cv)
%% \link[dest=cvq2-class]{cvq2} OR \linkS4class{cvq2}
  \subsection{\code{q2()}}{
    \cr
    The method \code{\link{q2}} returns an object of class \code{"\linkS4class{q2}"}.
    It contains information about the model calibration and its prediction performance on the external data set, \code{predictData}.
  }
  \subsection{\code{cvq2(), looq2()}}{
    \cr
    The methods \code{\link{cvq2}} and \code{\link{looq2}} return an object of class \code{"\linkS4class{cvq2}"}.
    It contains information about the model calibration and its prediction performance as well as data about the cross-validation applied to \code{modelData}.
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
  require(methods)
  require(stats)
  library(cvq2)
  
  data(cvq2.sample.A)
  result <- cvq2( cvq2.sample.A )
  result
  
  data(cvq2.sample.B)
  result <- cvq2( cvq2.sample.B, y ~ x, nFold = 3 )
  result
  
  data(cvq2.sample.B)
  result <- cvq2( cvq2.sample.B, y ~ x, nFold = 3, nRun = 5 )
  result
  
  data(cvq2.sample.A)
  result <- looq2( cvq2.sample.A, y ~ x1 + x2 )
  result
  
  data(cvq2.sample.A)
  data(cvq2.sample.A_pred)
  result <- q2( cvq2.sample.A, cvq2.sample.A, y ~ x1 + x2 )
  result 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% __ONLY ONE__ keyword per line
\keyword{ calibration performance }
\keyword{ cross validation }
\keyword{ cross-validation }
\keyword{ model calibration }
\keyword{ prediction performance }
\keyword{ prediction power }
\keyword{ predictive squared correlation coefficient }
\keyword{ q^2 }
\keyword{ q square }
\keyword{ root mean square error }
