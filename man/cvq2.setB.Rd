\name{cvq2.setB}
\alias{cvq2.setB}
\docType{data}
\title{
  Small data set to demonstrate the difference between the conventional and the predictive squared correlation coefficient while performing a cross-validation.
}
\description{
  Contains a small data set with six observations, the observed value \emph{y} depends on the parameter \eqn{x}{x}.
}
\usage{data(cvq2.setB)}
\format{
  A data frame with 6 observations and one parameter per observation.
  \describe{
    \item{\code{x}}{parameter}
    \item{\code{y}}{observation}
  }
}
\details{
  The prediction power can be determined with cross-validation.
  The cross-validation can be performed as Leave-One-Out (\eqn{\code{nFold} = \var{N} = 6}{nFold = N = 6}) or as k-fold (\eqn{\code{nFold} = {2, 3}}{nFold = {2, 3}}).\cr
  If \eqn{\code{nFold} = {2, 3}}{nFold = {2, 3}}), \code{modelData} is randomly split into \code{nFold} disjunct and equal sized (test) sets.
%  The parameters of cross-validation can be varied, e.g. the fold (\eqn{\code{nFold} = {2, 3, 6}}{nFold={2, 3, 6}).
%  Thereby \code{modelData} is randomly split into \code{nFold} disjunct and equal sized (test) sets.
  Furthermore, in that case one has the opportunity to repeat the cross-validation, while each run (\eqn{\code{nRun} = 2 \ldots x}{nRun=2\ldots x}) has an individual test set compilation.\cr
  The prediction power, \eqn{q^2_{cv}}{q^2_cv}, calculated for this data set is considerably smaller than the model calibration, \eqn{r^2}{r^2}, promises.
}
\source{
  Generic data set, created for this purpose only.
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
  data(cvq2.setB)
}
