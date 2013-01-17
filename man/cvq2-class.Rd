\name{cvq2-class}
\Rdversion{1.1}
\docType{class}
\alias{cvq2-class}
\alias{show,cvq2-method}

\title{Class \code{"cvq2"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
  The class \code{"cvq2"} is used to store information about the model calibration and its prediction performance.  
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cvq2", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    %hier jetzt die individuellen items
    \item{\code{result}}{
%%    cross validation results, mainly \eqn{q^2}{q^2}, \eqn{r^2}{r^2} and \eqn{rmse}{rmse}, districution of training and test set, \eqn{y_{mean}}{y_mean}
      Contains two lists (\code{fit}, \code{cv}) regarding the results from linear regression (model calibration, \code{fit}) and cross validation (prediction power, \code{cv}), see below.
    %names(result$fit)
    }
    \item{\code{output}}{A list of parameters like number formats, output restrictions or output targets.}
  }
%%  \cr
  
  \subsection{Linear regression results list}{
    % calibration performance
    \describe{
    %  
      \item{\code{datatable}}{The observed and predicted values.}
      \item{\code{datatable_columns}}{The explanation of the datatable's column names.}
    %
      \item{\code{model}}{The linear regression model.}
      \item{\code{n}}{The number of elements in the data set.}
      \item{\code{observed_mean}}{The arithmetic mean of the observed values.}
    % result  
      \item{\code{r2}}{The conventional squared correlation coefficient.}
      \item{\code{rmse}}{The linear regression root mean square error.}
    %% ...
    }
  }
  \subsection{Cross validation results list}{
  %prediction power
      \describe{
    %
      \item{\code{datatable}}{For each observed value, it contains the model parameters, the value is predicted with. Furthermore, the arithmetic mean of the training set and the predicted value are stored in this table.}
      \item{\code{datatable_columns}}{The explanation of the datatable's column names.}
    %run parameter
      \item{\code{decimalSplit}}{\emph{True}, if some groups (test sets) consist of \eqn{k-1}{k-1} elements.}
      \item{\code{nGroup}}{The number of individual sets generated from the data.}
      \item{\code{nRun}}{The number of runs each value is predicted.}
      \item{\code{nTestSet}}{The number of elements in one test set (\eqn{k}{k}).}
      \item{\code{nTrainingSet}}{The number of elements in one training set (\eqn{N-k}{N-k}).}
    %result
      \item{\code{q2}}{The predictive squared correlation coefficient.}
      \item{\code{rmse}}{The root mean square error. This will be calculated with Bessel's sample covariance correction, using \eqn{N-1}{N-1} in the denominator instead \eqn{N}{N} 
    %  According to Bessel's sample covariance correction, the root mean square error will be calculated using \eqn{N-1}}{N-1} in the denominator instead \eqn{N}}{N}. 
      }
      \item{\code{TestSet}}{The composition of the individual test sets.}
    }
  }
}
\section{Methods}{
  \describe{
    \item{show}{Returns a comprehensive overview about the model calibration and the prediction performance.}
	 }
}
%%\references{
%%  ~~put references to the literature/web site here~~
%%}
\author{
  Torsten Thalheim <torstenthalheim@gmx.de>
}
%%\note{}
%%\section{ANY}{}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
  \code{\link{cvq2}}
}
\examples{
  showClass("cvq2")
}
\keyword{classes}
