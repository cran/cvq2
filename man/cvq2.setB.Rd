\name{cvq2.setB}
\alias{cvq2.setB}
\docType{data}
\title{
  Small data set to demonstrate the difference between the conventional and the predictive squared correlation coefficient while performing a cross validation.
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
  The cross validation can be performed with various settings for this data set.
  This includes several individual runs (\eqn{\code{nRun} = 2 \ldots x}{nRun=2\ldots x}) with various compilations of training and data sets. 
  Furthermore the number of test sets can be adjusted (\eqn{\code{nGroup} =  1 \ldots 3}{nGroup=1\ldots 3}).
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
## maybe str(cvq2.setB) ; plot(cvq2.setB) ...
}
\keyword{datasets}
