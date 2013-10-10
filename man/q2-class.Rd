\name{q2-class}
\Rdversion{1.1}
\docType{class}
\alias{q2-class}
\alias{show,q2-method}

\title{Class \code{"q2"}}
\description{
  The class \code{"q2"} is used to store information about the calibration of model \var{M} and its prediction performance.
  To determine the prediction power, \var{M} is applied to an external, independent data set.
%  To demonstrate the prediction power \var{M} is applied to an external, independent data set.  
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("q2", \ldots)}.
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
  
  \subsection{Model calibration}{
    This part contains the measurements regarding the model calibration of the linear model \var{M}.
    % calibration performance
    \describe{
    %  
      \item{\code{data}}{The observations and the linear fitted predictions by model \var{M}}
      \item{\code{data.col}}{The explanation of data's column names}
    %
      \item{\code{model}}{The linear model \var{M}}
      \item{\code{n}}{The number of elements in the data set}
      \item{\code{observed_mean}}{The arithmetic mean of the observations}
    % result  
      \item{\code{r2}}{The conventional squared correlation coefficient}
      \item{\code{rmse}}{The root mean square error with regard to the degree's of freedom \eqn{\nu}{\nu}}
      \item{\code{nu}}{The degree's of freedom}
    %% ...
    }
  }
  \subsection{Prediction performance}{
    This part contains the measurements regarding the prediction power of model \var{M} which ia applied to an external data set.
  %prediction power
    \describe{
    %
      \item{\code{data}}{Contains the observations and their predictions by \var{M}}
      \item{\code{data.col}}{The explanation of data's column names}
      \item{\code{nTrainingSet}}{The number of elements in the model set (\eqn{N-k}{N-k})}
      \item{\code{nTestSet}}{The number of elements in the prediction set (\var{k})}
    %result
      \item{\code{q2}}{The predictive squared correlation coefficient}
      \item{\code{rmse}}{The root mean square with regard to the degree's of freedom \eqn{\nu}{\nu}}
      \item{\code{nu}}{The degree's of freedom}
    %  According to Bessel's sample covariance correction, the root mean square error will be calculated using \eqn{N-1}}{N-1} in the denominator instead \eqn{N}}{N}
%References Bessel's correction
%http://mathworld.wolfram.com/BesselsCorrection.html
%    Farebrother, R. W. Fitting Linear Relationships: A History of the Calculus of Observations 1750-1900. New York: Springer-Verlag, 1999.
%Gauss, C. F. "Theoria combinationis obsevationum erroribus minimis obnoxiae." Werke, Vol. 4. Goettingen, Germany: p. 1, 1823.
%Kenney, J. F. and Keeping, E. S. Mathematics of Statistics, Pt. 2, 2nd ed. Princeton, NJ: Van Nostrand, 1951. 
% Oxford University Press, Dictionary of statistics, online available from university 
% http://www.oxfordreference.com
% http://www.oxfordreference.com/view/10.1093/acref/9780199541454.001.0001/acref-9780199541454-e-1704?rskey=Vlbjn7&result=2159 
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
