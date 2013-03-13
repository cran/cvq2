\name{cvq2.setA}
\alias{cvq2.setA}
\docType{data}
%do not use \eqn in title
\title{
  Small data set to demonstrate the difference between the conventional and the predictive squared correlation coefficient.
}
\description{
  Contains a small data set with four observations, the observed value \emph{y} depends on two parameters (\eqn{x_1}{x_1}, \eqn{x_2}{x_2}).
%  If a cross-validation is applied to the data set, the resulting \eqn{q^2_{cv}}{q^2_cv} value is much smaller than the \eqn{r^2}{r^2} value.    
}
\usage{data(cvq2.setA)}
\format{
  A data frame with four observations. 
  Each row contains two parameters and the observed value.
  \describe{
  %not possible, to use \eqn here
    \item{\code{x1}}{parameter 1}
    \item{\code{x2}}{parameter 2}
    \item{\code{y}}{observed value}
  }
}
%% glm for cvq2
%% cvq2.setA$x1*0.1521+cvq2.setA$x2*1.0621+0.715
%% [1] 1.408771 1.985711 4.077645 4.941352
%% plot(cvq2.setA$x1*0.1521+cvq2.setA$x2*1.0621+0.715, cvq2.setA$y)
\details{
  This data set can be used to demonstrate the differences between the model calibration and the prediction power.
  The prediction power can be determined either with cross-validation or the application of the model to the data set \code{\link{cvq2.setA_pred}}.
}
\note{
  This data set contains one outlier (row 2).
  If the prediction power is determined with cross-validation, this outlier leads to a considerably decreased prediction power, \eqn{q^2_{cv}}{q^2_cv}, compared to the model calibration, \eqn{r^2}{r^2}.
  For this data set, one can perform a Leave-One-Out cross-validation only.
%%  Due to the limited number of observations (\emph{4}), the impact of the outlier can not be compensated by the other observations. 
%%  If the prediction power is evaluated with cross-validation, one can perform a Leave-One-Out cross-validation only with this data set.
}
\source{
  Generic data set, created for this purpose only. 
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
  data(cvq2.setA)
}
