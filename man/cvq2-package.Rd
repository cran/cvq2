\name{cvq2-package}
\alias{cvq2-package}
\docType{package}
\title{
Calculate the predictive squared correlation coefficient.
}
\description{
For small data sets, the calculation of the predictive squared correlation coefficient, q^2, is more accurate than the calculation of the conventional squared correlation coefficient, r^2.
}
\details{
\tabular{ll}{
Package: \tab cvq2\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-09-22\cr
License: \tab GPL v3\cr
LazyLoad: \tab yes\cr
}
This package can be used to calculate the predictive squared correlation coefficient, q^2. 
\strong{Currently, the calculation of leave-one-out cross validation is implemented only.}
}
\author{
Torsten Thalheim <torstenthalheim@gmx.de>
}
\references{
Schuurmann G, Ebert R-U, Chen J, Wang B, Kuhne R. 2008. External validation and prediction employing the predictive squared correlation coefficient - test set activity mean vs training set activity mean. \emph{J. Chem. Inf. Model.} \bold{48:} 2140-2145 
}
\keyword{
  q^2
  q square
  predictive squared correlation coefficient 
}
%%\seealso{}
\examples{
library(cvq2)
data(cvq2.setA)
result <- cvq2( cvq2.setA, y~x_1+x_2)
result
}