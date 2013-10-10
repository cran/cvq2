\name{cvq2.sample.A_pred}
\alias{cvq2.sample.A_pred}
\docType{data}
\title{
  Prediction set for model set \code{\link{cvq2.sample.A}}.
}
\description{
  This data set can be used to determine the prediction power of the model \var{M} generated with \code{\link{cvq2.sample.A}}.
  The four observations \var{y} depend on two parameters (\eqn{x_1}{x_1}, \eqn{x_2}{x_2}).
}
\usage{data(cvq2.sample.A_pred)}
\format{
  A data frame with four observations. 
  Each row contains two parameters and the observed value.
  \describe{
    \item{\code{x1}}{parameter 1}
    \item{\code{x2}}{parameter 2}
    \item{\code{y}}{observation}
  }
}
\details{
%%  This data set can be used to determine the prediction power of the model \code{\link{cvq2.sample.A}}.
  This data set fits very good to the model \var{M} derived from \code{\link{cvq2.sample.A}}.
  The prediction power \eqn{q^2}{q^2} of \var{M} for \code{cvq2.sample.A_pred} is as high as its calibration power, \eqn{r^2}{r^2}.
}
\source{
  Generic data set, created for this purpose only. 
}
\seealso{
  \code{\link[=cvq2-package]{cvq2}},
  \code{\link{q2}}
}
%\examples{
%  data(cvq2.sample.A_pred)
%}
