\name{cvq2.sample.B}
\alias{cvq2.sample.B}
\docType{data}
\title{
  Small data set to demonstrate the difference between the conventional and the predictive squared correlation coefficient while performing a cross-validation.
}
\description{
  Contains a small data set with six observations, the observed value \emph{y} depends on the parameter \eqn{x}{x}.
}
\usage{data(cvq2.sample.B)}
\format{
  A data frame with six observations and one parameter per observation.
  \describe{
    \item{\code{x}}{parameter}
    \item{\code{y}}{observation}
  }
}
\details{
  The sample can be used to demonstrate the various settings of \code{cvq2}.
  The cross-validation applied to determine \eqn{q^2}{q^2} can be performed either as Leave-One-Out (\eqn{\code{nFold} = \var{N} = 6}{nFold = N = 6}) or as k-fold (\eqn{\code{nFold} = \lbrace2, 3\rbrace}{nFold = 2,3}).\cr
  In case \eqn{\code{nFold} = \lbrace 2, 3\rbrace}{nFold = 2,3} \code{modelData} is randomly split into \code{nFold} disjunct and equal sized test sets.
  Furthermore one has the opportunity to repeat the cross-validation, while each run (\eqn{\code{nRun} = \lbrace 2, 3, \ldots, x\rbrace}{nRun=2,3\ldots, x}) has an individual test set compilation.\cr
  The prediction power, \eqn{q^2_{cv}}{q^2_cv}, calculated for this data set is considerably smaller than the model calibration, \eqn{r^2}{r^2}, promises.
%  The parameters of cross-validation can be varied, e.g. the fold (\eqn{\code{nFold} = {2, 3, 6}}{nFold={2, 3, 6}).
%  Thereby \code{modelData} is randomly split into \code{nFold} disjunct and equal sized (test) sets.
}
\source{
  Generic data set, created for this purpose only.
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
%\examples{
%  data(cvq2.sample.B)
%}
\seealso{
  \code{\link[=cvq2-package]{cvq2}},
  \code{\link{q2}}
}
\keyword{datasets}
