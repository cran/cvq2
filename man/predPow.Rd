\name{predPow}
\alias{predPow}
\alias{predictionPower}
\alias{calibPow}
\alias{calibrationPower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Statistical analysis of a model results compared to observations.                          
}
\description{
  Determines the model calibration or its prediction power. 
  The statistical analysis is done with the observed values and their related prediction only, as no data about the model used to calculate the prediction is available.
  %In case no data about the model used to calculate the prediction is available.
}
\usage{
  calibPow(data, obs = "observed", pred = "predicted", 
  nu = 0, round = 4, extOut = FALSE, extOutFile = NULL)
  predPow(data, obs = "observed", pred = "predicted", 
  obs_mean = NULL, nu = 0, round = 4, extOut = FALSE, 
  extOutFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    A data frame that contains at least two columns containing the observations and their predictions. 
    The data frame can be extended e.g. by a column that specifies the individual mean of the observed values \eqn{y_{mean}^{N-k,i}}{y_{mean}^{N-k,i}}.
}
  \item{obs}{
    The name of the column that contains the observations
  }
  \item{pred}{
    The name of the column that contains the predictions
  }
  \item{obs_mean}{
    The mean of the observations \eqn{y_{mean}^{N-k,i}}{y_{mean}^{N-k,i}}. 
    Can be either a string that names the actual column or the column itself
  }
  \item{nu}{
    The degrees of freedom used for \var{rmse} calculation, \strong{DEFAULT: 0}
  }
  \item{round}{
    The rounding value used in the output, \strong{DEFAULT: 4}
  }
  \item{extOut}{
    Extended output, \strong{DEFAULT: FALSE}\cr
    If \code{extOutFile} is not specified, write to stdout()
  }
  \item{extOutFile}{
    Write extended output into file (\emph{implies} \code{extOut = TRUE}), \strong{DEFAULT: NULL}
  }
}
\details{
  \code{data} contains the observation and the its predictions calculated with model \var{M}.
  \subsection{\code{calibPow()}}{
    \cr
    \emph{Alias:} \code{calibrationPower()}\cr
    The calibration power of model \var{M} is calculated with \code{data}.
  }
  \subsection{\code{predPow()}}{
    \cr
    \emph{Alias:} \code{predictionPower()}\cr
    The prediction power of model \var{M} is calculated with \code{data}.
  }
}
\value{
  Returns an object of class \code{"\linkS4class{q2}"}.
  It contains information about the model calibration or its prediction performance.
}
%%\references{}
\author{
  Torsten Thalheim <torstenthalheim@gmx.de>
}
%%\note{}
%%\seealso{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
  require(methods)
  require(stats)
  library(cvq2)
  
  data(cvq2.sample.C)
  result <- calibPow( cvq2.sample.C )
  result
  
  data(cvq2.sample.D)
  result <- predPow( cvq2.sample.D, obs_mean="observed_mean" )
  result
}
\seealso{
  \code{\link[=cvq2-package]{cvq2}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% __ONLY ONE__ keyword per line
\keyword{ calibration performance }
\keyword{ model calibration }
\keyword{ prediction performance }
\keyword{ prediction power }
\keyword{ predictive squared correlation coefficient }
\keyword{ q^2 }
\keyword{ q square }
\keyword{ root mean square error }
