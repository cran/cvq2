\name{cvq2-class}
\Rdversion{1.1}
\docType{class}
\alias{cvq2-class}
\alias{show,cvq2-method}

\title{Class \code{"cvq2"}}
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
\description{
  The class \code{"cvq2"} extends class \code{"\linkS4class{q2}"} and is used to store information about the model calibration, its prediction performance and the cross-validation applied to determine the prediction performance.
%  The prediction performance is calculated with a cross-validation.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("cvq2", ...)}.
}
\section{Slots}{
  \describe{
    %hier jetzt die individuellen items
    \item{\code{result}}{
%%    cross-validation results, mainly \eqn{q^2}{q^2}, \eqn{r^2}{r^2} and \eqn{rmse}{rmse}, districution of training and test set, \eqn{y_{mean}}{y_mean}
      Contains three lists (\code{fit}, \code{pred}, \code{cv}) regarding the results from linear regression (model calibration, \code{fit}) and cross-validation (prediction power, \code{pred} and \code{cv}) for the given model.
    %names(result$fit)
    }
    \item{\code{output}}{A list of parameters like number formats, output restrictions or output targets.}
  }
%%  \cr
  \subsection{Linear regression and prediction result list}{
    These lists are inherted from the parent class \code{\linkS4class{q2}}.
    Differences caused by cross-validation appear in the prediction result list for:
    \describe{
      \item{\code{data}}{For each observation the model parameters used for the prediction are stored additionally as well as the arithmetic mean of the training set
      }
      \item{\code{nTrainingSet}}{The number of elements in \emph{one} training set (\eqn{N-k}{N-k}) plus an eventually variation.}
      \item{\code{nTestSet}}{The number of elements in \emph{one} test set (\eqn{k}{k}) minus an eventually variation.}
    }

  }
  \subsection{Cross-validation result list}{
   %cross-validation
    \describe{
     %run parameter
      \item{\code{testSetSizeVaries}}{\emph{True}, if some test sets consist of \eqn{k-1}{k-1} elements.}
      \item{\code{nFold}}{\code{modelData} is randomly split into \var{n} equal sized (according to \code{testSetSizeVaries}) test sets for each individual run.}  %The data set is split into this number of individual test sets
      \item{\code{nRun}}{The number of runs each value is predicted.}
    }
  }
}
\section{Extends}{
  Class \code{"\linkS4class{q2}"}, directly.
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

%%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%%  \code{\link{q2}}
%%  \code{\linkS4class{q2}}
%%}
\examples{
  showClass("cvq2")
}
\keyword{classes}
