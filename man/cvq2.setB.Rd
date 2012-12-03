\name{cvq2.setB}
\alias{cvq2.setB}
\docType{data}
\title{
Small data set to demonstrate the difference between the conventional squared correlation coefficient and the predictive squared correlation coefficient while performing a cross validation.
}
\description{
Contains a small data set with six observations, the observed value \emph{y} depends on the parameter \eqn{x}{x}.
If a cross validation is applied to the data set, the resulting \eqn{q^2_{cv}}{q^2_cv} value is much smaller than the \eqn{r^2}{r^2} value.
The cross validation can be performed with various settings, like multiple runs (\code{nRun = 2\ldots x}) with various compilations of training and data sets. 
Furthermore the number of test sets can be adjusted (\code{nGroup =  1\ldots 3}).     
}
\usage{data(cvq2.setB)}
\format{
  A data frame with 6 observations and one parameter per observation.
  \describe{
    \item{\code{x}}{parameter}
    \item{\code{y}}{observation}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
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
