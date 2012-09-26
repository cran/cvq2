\name{cvq2}
\alias{cvq2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform calculation of the predictive squared correlation coefficient, q^2.
}
\description{
Calculate the predictive squared correlation coefficient. 
Therefore a cross validation is done, while splitting the dataset into a training set and a test set.
For the training set, the mean of the observed value (\eqn{y_{mean}^{training}}{y_mean^training}) is calculated and used for the predictive squared correlation coefficient, instead the mean of the observed value in the data set (\eqn{y_{mean}}{y_mean}).
\strong{Currently, the calculation of leave-one-out cross validation is implemented only.} 
}
\usage{
cvq2(data, formula, round = 4, outFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ 
  The data set used for cross validation, consists of parameter x_1,x_2,...,x_n and an observed value y
}
  \item{formula}{
  The formula used to predict the observed value, like y ~ x_1 + x_2 + ... + x_n
  %, DEFAULT: NULL
}
  \item{round}{
  The rounding value, DEFAULT: 4
}
  \item{outFile}{
  write output directly into outFile instead stdout(), DEFAULT: ""
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
%% FORMEL BESCHREIBEN
Implements the q^2 equation and its parameters:
\eqn{q^2_{tr} = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}^{training}\right)^2} }{ q_tr^2 = 1 - (SIGMA_i=1^N (y_i^pred - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean^training)^2)}
}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Torsten Thalheim <torstenthalheim@gmx.de>
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{}
\examples{
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

library(cvq2)
data(cvq2.setA)
result <- cvq2( cvq2.setA, y~x_1+x_2)
result

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% __ONLY ONE__ keyword per line
\keyword{ q^2 }
\keyword{ q square }
\keyword{ predictive squared correlation coefficient }
