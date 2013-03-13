\name{cvq2.setA_pred}
\alias{cvq2.setA_pred}
\docType{data}
\title{
  Prediction set for model set \code{\link{cvq2.setA}}.
}
\description{
  This data set can be used to determine the prediction power of the model \code{\link{cvq2.setA}}.
}
\usage{data(cvq2.setA_pred)}
\format{
  A data frame with four observations. 
  Each row contains two parameters and the observed value.
  \describe{
    \item{\code{x1}}{parameter 1}
    \item{\code{x2}}{parameter 2}
    \item{\code{y}}{observed value}
  }
}
\details{
%%  This data set can be used to determine the prediction power of the model \code{\link{cvq2.setA}}.
  The prediction set fits very good to the model set \code{\link{cvq2.setA}}, \eqn{q^2}{q^2} is as high as \eqn{r^2}{r^2}.
}
\source{
  Generic data set, created for this purpose only. 
}
\examples{
  data(cvq2.setA_pred)
}
