\name{cvq2-package}
\alias{cvq2-package}
\docType{package}
\encoding{latin1}
\title{
  Calculate the predictive squared correlation coefficient.
}
\description{
  This package compares observation with their predictions calculated by model \var{M}.
  It calculates the predictive squared correlation coefficient, \eqn{q^2}{q^2}, in comparison to the well known conventional squared correlation coefficient, \eqn{r^2}{r^2}.
%  In case the linear model \var{M} is generated on the fly, \eqn{q^2}{q^2} indicates the prediction performance of \var{M}, whereas \eqn{r^2}{r^2} is a measure for its calibration performance.
%  For a given model \var{M}, \eqn{q^2}{q^2} indicates the prediction performance of \var{M}, whereas \eqn{r^2}{r^2} is a measure for its calibration performance.
%  The model itself is generated on the fly (linear regression).
%The prediction performance of a model can be indicated with \eqn{q^2}{q^2}, whereas \eqn{r^2}{r^2} is a measure for the calibration performance of a model.
}
\details{
\tabular{ll}{
  Package: \tab cvq2\cr
  Type: \tab Package\cr
  Version: \tab 1.2.0 \cr
  Date: \tab 2013-10-10\cr
  % methods == s3 class, stats == glm
  Depends: \tab methods, stats\cr
  License: \tab GPL v3\cr
  LazyLoad: \tab yes\cr
}
This package needs either a description of parameters and observations (I) or a data set that already contains the observations and their related predictions (II).
In case of (I), a linear model \var{M} is generated on the fly. 
Afterwards, its calibration performance can be compared with its prediction power.
%In case of (I), this package generates a linear model \var{M} on the fly and compares its calibration performance with its prediction power (I).
If the input data consist of observations and precidctions only (II), the package can be used to compute either the calibration performance or the prediction power.
%% FORMEL BESCHREIBEN
% y_fit: r^2 - DataSet + External TestSet, Vorhersagewerte aus N Beobachtungen DataSet, y_mean aus y(DataSet)
% y_pred: q^2 - DataSet + External TestSet, Vorhersagewerte aus N-1 Beobachtungen, exklusive der i-ten Beobachtung, jeder Wert aus TestSet wird N-mal vorhergesagt(?), y_mean ist das Gleiche wie fuer y_fit -> y(DataSet)
% y_pred: q^2_tr - DataSet + External TestSet, Vorhersagewerte aus N-1 Beobachtungen, exklusive der i-ten Beobachtung, y_mean fuer N-1 y-Werte aus dem Trainingsset 
% y_pred(N-k): q^2_cv - DataSet -> TrainingSet + TestSet - vorhergesagte Werte Testset, Parameter werden aus Trainingset generiert, y_mean fuer N-k y-Werte aus dem Trainingsset
%U+2261 \u2661 kongruent, \u2263 - 4fach Gleichheitszeichen
%For \var{M}, a general linear regression is performed to calculate the conventional squared correlation coefficient, \eqn{r^2}{r^2}:
If model \var{M} is generated on the fly (I), the procedure is as follows:
The input data set consists of parameters \eqn{x_1, x_2, \ldots, x_n}{x_1, x_2 \ldots, x_n} which describe an observation \var{y}.
A linear regression (\code{\link[stats:glm]{glm}}) of this data set yields to \var{M}.
Thus the conventional squared correlation coefficient, \eqn{r^2}{r^2}, can be calculated:
\deqn{r^2 = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{fit} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}\right)^2} \equiv 1 - \frac{RSS}{SS}}{ q^2 = 1 - (SIGMA_i=1^N (y_i^fit - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean)^2) \u2261 1 - RSS/SS}
%The observed values \eqn{y_i}{y_i} are compared to the fitted values \eqn{y_i^{fit}}{y_i^fit}.
%Those values were determined with a linear regression and yield to the calibration performance, \eqn{r^2}{r^2}, of the described model \var{M}. 
The denominator complies with the \strong{R}esidual \strong{S}um of \strong{S}quares \emph{RSS}, the difference between the fitted values \eqn{y_i^{fit}}{y_i^fit} predicted by \var{M} and the observations \eqn{y_i}{y_i}.
The numerator is the \strong{S}um of \strong{S}quares, \emph{SS}, and refers to the difference between the observations \eqn{y_i}{y_i} and their mean \eqn{y_{mean}}{y_mean}. \cr
To compare the calibration of \var{M} with its prediction power, \var{M} is applied to an external data set. 
External it is called, because these data have not been used during the linear regression to generate \var{M}.
The comparison of the predictions \eqn{y_i^{pred}}{y_i^pred} with the observation \eqn{y_i}{y_i} yields to the predictive squared correlation coefficient, \eqn{q^2}{q^2}:  
\deqn{q^2 = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}\right)^2} \equiv 1 - \frac{PRESS}{SS}}{ q^2 = 1 - (SIGMA_i=1^N (y_i^pred - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean)^2) \u2261 1 - PRESS/SS}
The \strong{PRE}dictive residual \strong{S}um of \strong{S}quares \emph{PRESS} is the difference between the predictions \eqn{y_i^{pred}}{y_i^pred} and the observations \eqn{y_i}{y_i}.
The \strong{S}um of \strong{S}quares \emph{SS} refers to the difference between the observations \eqn{y_i}{y_i} and their mean \eqn{y_{mean}}{y_mean}.
%, not the arithemtic mean obtained from the observed values in the initial data set.
%To avoid any bias, \eqn{y_{mean}}{y_mean} is the arithemtic mean of the \eqn{y_i}{y_i} from the _external data_ #vs y_mean^training#  set.
%Hence the clarifying \eqn{q^2_{tr}}{q^2_tr} equation is slighlty different to the previous \eqn{q^2}{q^2} equation:
%\deqn{q^2_{tr} = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}^{training}\right)^2} }{ q_tr^2 = 1 - (SIGMA_i=1^N (y_i^pred - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean^training)^2)}
%The arithmetic mean of the observed values in the _external data_ #vs y_mean^training# set, \eqn{y_{mean}^{training}}{y_mean^training}, is used to determine the prediction performance, \eqn{q^2_{tr}}{q^2_tr}, of \var{M}.

In case that no external data set is available, one can perform a cross-validation to evaluate the prediction performance.
The cross-validation splits the model data set (\eqn{N}{N} elements) into a training set (\eqn{N-k}{N-k} elements) and a test set (\eqn{k}{k} elements). 
Each training set yields to an individual model \var{M'}, which is used to predict the missing \eqn{k}{k} value(s).
Each model \var{M'} is slightly different to \var{M}.
Thereby any observed value \eqn{y_i}{y_i} is predicted once and the comparison between the observation and the prediction (\eqn{y_i^{pred(N-k)}}{y_i^{pred(N-k)}}) yields to \eqn{q^2_{cv}}{q^2_cv}:
\deqn{q^2_{cv} = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred(N-k)} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}^{N-k,i}\right)^2} }{ q_cv^2 = 1 - SIGMA_i=1^N (y_i^pred(N-k) - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean^(N-k,i)^2} 
The arithmetic mean used in this equation, \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, is individually for any test set and calculated for the observed values comprised in the training set.

If \eqn{k > 1}{k>1}, the compilation of training and test set may have impact on the calculation of the predictive squared correlation coefficient.
To overcome biasing, one can repeat this calculation with various compilations of training and test set. 
Thus, any observed value is predicted several times, according to the number of runs performed.
% comprised == beinhalten, ginge auch contain, involve, imply, available from
Remark, if the prediction performance is evaluated with cross-validation, the calculation of the predictive squared correlation coefficient, \eqn{q^2}{q^2}, is more accurate than the calculation of the conventional squared correlation coefficient, \eqn{r^2}{r^2}.

%A further method to measure the calibration and prediction performance is the root-mean-square-error \var{rmse}. 
In addition to \eqn{r^2}{r^2} and \eqn{q^2}{q^2} the root-mean-square-error \var{rmse} is calculated to measure the accuracy of model \var{M}:
\deqn{rmse = \sqrt{\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{N-\nu}}}
The \var{rmse} ist the difference between a model's prediction (\eqn{y_i^{pred}}{y_i^{pred}}) and the actual observation (\eqn{y_i}{y_i}) and can be applied for both, calibration and prediction power.
It depends on the number of observations \var{N} and the method used to generate the model \var{M}.
The \var{rmse} tends to overestimate \var{M}.
According to Friedrich Bessel's suggestion [Upton and Cook 2008], this overestimation can be resolved while regarding the degrees of freedom, \eqn{\nu}{\nu}. 
Thus in case of cross-validation, \eqn{\nu = 1}{\nu=1} is recommended to calculate the \var{rmse} in relation to the prediction power.
The degrees of freedom, \eqn{\nu}{\nu}, for the calculation of \var{rmse} regarding the prediction power can be set as parameter for \code{cvq2()}, \code{looq2()} and \code{q2()}.
In opposite \eqn{\nu = 0}{\nu=0} is fixed while calculating the \var{rmse} in relation to the model calibration.

%this is actually user knowledge - soll man das schreiben
In case, the input is a comparison of observed and predicted values only (II), \eqn{r^2}{r^2} respective \eqn{q^2}{q^2} as well as their \var{rmse} are calculated immediately for these data. Neither a model \var{M} is generated nor a cross-validation is applied.
}
\author{
  Torsten Thalheim <torstenthalheim@gmx.de>
}
\note{
%  This package was developed to support my colleagues at the...
%  This package was initiated the Ecological Chemistry Department during my time at the Helmholtz Centre for Environmental Research in Leipzig.
  The package development started few years ago in the Ecological Chemistry Department during my time at the Helmholtz Centre for Environmental Research in Leipzig.
  Thereby it is based on \enc{Schüürmann}{Schuurmann} et al. 2008: External validation and prediction employing the predictive squared correlation coefficient - test set activity mean vs training set activity mean. 
}
\references{
  \enumerate{
    \item Cramer RD III. 1980. BC(DEF) Parameters. 2. An Empirical Structure-Based Scheme for the Prediction of Some Physical Properties. \emph{J. Am. Chem. Soc.} \bold{102:} 1849-1859.
    \item Cramer RD III, Bunce JD, Patterson DE, Frank IE. 1988. Crossvalidation, Bootstrapping, and Partial Least Squares Compared with Multiple Linear Regression in Conventional QSAR Studies. \emph{Quant. Struct.-Act. Relat.} \bold{1988:} 18-25.
%    \item Reichmann, WJ. 1961. Use and abuse of statistics. \emph{Oxford University Press}. Appendix VIII
    \item Organisation for Economic Co-operation and Development. 2007. Guidance document on the validation of (quantitative) structure-activity relationship [(Q)SAR] models. \emph{OECD Series on Testing and Assessment 69.} OECD Document ENV/JM/MONO(2007)2, pp 55 (paragraph no. 198) and 65 (Table 5.7).
    \item \enc{Schüürmann}{Schuurmann} G, Ebert R-U, Chen J, Wang B, \enc{Kühne}{Kuhne} R. 2008. External validation and prediction employing the predictive squared correlation coefficient - test set activity mean vs training set activity mean. \emph{J. Chem. Inf. Model.} \bold{48:} 2140-2145.
    \item Tropsha A, Gramatica P, Gombar VK. 2003. The Importance of Being Earnest: Validation is the Absolute Essential for Successful Application and Interpretation of QSPR Models. \emph{QSAR Comb. Sci.} \bold{22:} 69-77.
    % siehe auch
    % http://www.oxfordreference.com
    % http://www.oxfordreference.com/view/10.1093/acref/9780199541454.001.0001/acref-9780199541454-e-1704?rskey=Vlbjn7&result=2159 
    \item Upton G, Cook I. 2008. Oxford Dictionary of Statistics \emph{Oxford University Press} \bold{ISBN 978-0-19-954145-4} entry for "Variance (data)". 
  }
}                                                                     
% __ONLY ONE__ keyword per line
\keyword{ calibration performance }
\keyword{ cross validation }
\keyword{ cross-validation }
\keyword{ model calibration }
\keyword{ prediction performance }
\keyword{ prediction power }
\keyword{ predictive squared correlation coefficient }
\keyword{ q^2 }
\keyword{ q square }
\keyword{ root mean square error }
%%\seealso{}
\examples{
  library(cvq2)
  
  data(cvq2.sample.A)
  result <- cvq2( cvq2.sample.A, y ~ x1 + x2 )
  result
  
  data(cvq2.sample.B)
  result <- cvq2( cvq2.sample.B, y ~ x, nFold = 3 )
  result
  
  data(cvq2.sample.B)
  result <- cvq2( cvq2.sample.B, y ~ x, nFold = 3, nRun = 5 )
  result
  
  data(cvq2.sample.A)
  data(cvq2.sample.A_pred)
  result <- q2( cvq2.sample.A, cvq2.sample.A_pred, y ~ x1 + x2 )
  result
  
  data(cvq2.sample.C)
  result <- calibPow( cvq2.sample.C )
  result
  
  data(cvq2.sample.D)
  result <- predPow( cvq2.sample.D, obs_mean="observed_mean" )
  result
}
