\name{cvq2.sample.D}
\alias{cvq2.sample.D}
\docType{data}
\title{
  Small data set to demonstrate the statistic methods.
}
\description{
  Contains a small data set with four observations, four predictions and the arithmetic mean of the observed values used for each prediction.
}
\usage{data(cvq2.sample.D)}
\format{
  A data frame with four observations, four predictions and four different arithmetic means \eqn{y_{mean}^{N-k,i}}{y_{mean}^{N-k,i}} (see \link{cvq2-package}).
  \describe{
    \item{\code{observed}}{observation}
    \item{\code{predicted}}{prediction}
    \item{\code{observed_mean}}{mean of the observed values used during the prediction}
  }
}
%\details{}
\source{
  Generic data set, created for this purpose only.
}
%\references{}
%\examples{
%  data(cvq2.sample.D)
%}
\seealso{
  \code{\link[=cvq2-package]{cvq2}},
  \code{\link{predPow}}
  }
\keyword{datasets}
