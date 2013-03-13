\name{q2-class}
\Rdversion{1.1}
\docType{class}
\alias{q2-class}
\alias{show,q2-method}

\title{Class \code{"q2"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
  The class \code{"q2"} is used to store information about the model calibration and its prediction performance demonstrated by an external data set.  
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("q2", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    %hier jetzt die individuellen items
    \item{\code{result}}{
%%    cross-validation results, mainly \eqn{q^2}{q^2}, \eqn{r^2}{r^2} and \eqn{rmse}{rmse}, districution of training and test set, \eqn{y_{mean}}{y_mean}
      Contains two lists (\code{fit}, \code{pred}) regarding the results from linear regression (model calibration, \code{fit}) and the application of the model to a validation set (prediction power, \code{pred})
    %names(result$fit)
    }
    \item{\code{output}}{A list of parameters like number formats, output restrictions or output targets}
  }
%%  \cr
  
  \subsection{Linear regression result list}{
    % calibration performance
    \describe{
    %  
      \item{\code{datatable}}{The observed and predicted values}
      \item{\code{datatable_columns}}{The explanation of the datatable's column names}
    %
      \item{\code{model}}{The linear regression model}
      \item{\code{n}}{The number of elements in the data set}
      \item{\code{observed_mean}}{The arithmetic mean of the observed values}
    % result  
      \item{\code{r2}}{The conventional squared correlation coefficient}
      \item{\code{rmse}}{The root mean square error}
    %% ...
    }
  }
  \subsection{Prediction result list}{
  %prediction power
      \describe{
    %
      \item{\code{datatable}}{Contains the observed value and its prediction by the model}
      \item{\code{datatable_columns}}{The explanation of the datatable's column names}
      \item{\code{nTrainingSet}}{The number of elements in the model set (\eqn{N-k}{N-k})}
      \item{\code{nTestSet}}{The number of elements in the prediction set (\eqn{k}{k})}
    %result
      \item{\code{q2}}{The predictive squared correlation coefficient}
      \item{\code{rmse}}{The root mean square error is calculated with Bessel's sample covariance correction, using \eqn{N-1}{N-1} in the denominator instead \eqn{N}{N} 
    %  According to Bessel's sample covariance correction, the root mean square error will be calculated using \eqn{N-1}}{N-1} in the denominator instead \eqn{N}}{N}
      }
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
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%%}
\examples{
showClass("q2")
}
\keyword{classes}
