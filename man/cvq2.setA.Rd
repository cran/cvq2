\name{cvq2.setA}
\alias{cvq2.setA}
\docType{data}
%do not use \eqn in title
\title{
  Small data set to demonstrate the difference between the conventional and the predictive squared correlation coefficient while performing a Leave-One-Out cross validation.
}
\description{
  Contains a small data set with four observations, the observed value \emph{y} depends on two parameters (\eqn{x_1}{x_1}, \eqn{x_2}{x_2}).
%  If a cross validation is applied to the data set, the resulting \eqn{q^2_{cv}}{q^2_cv} value is much smaller than the \eqn{r^2}{r^2} value.    
}
\usage{data(cvq2.setA)}
\format{
  A data frame with four observations. Each row contains two parameters and the observed value.
  \describe{
  %not possible, to use \eqn here
    \item{\code{x1}}{parameter 1}
    \item{\code{x2}}{parameter 2}
    \item{\code{y}}{observed value}
  }
}
\details{
  This data set contains one outlier (row 3).
  If the data set is used for modeling, this outlier leads to a considerably decreased prediction power, \eqn{q^2_{cv}}{q^2_cv}, compared to the model calibration, \eqn{r^2}{r^2}.
  Due to the limited number of observations (\emph{4}), the impact of the outlier can not be compensated by the other observations. 
}
\note{
  With this data set, one can perform a Leave-One-Out cross validation only.
}
\source{
  Generic data set, created for this purpose only. 
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
  data(cvq2.setA)
## maybe str(cvq2.setA) ; plot(cvq2.setA) ...
}
\keyword{datasets}
