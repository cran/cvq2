\name{cvq2-package}
\alias{cvq2-package}
\docType{package}
\encoding{latin1}
\title{
Calculate the predictive squared correlation coefficient.
}
\description{
This package calculates the predictive squared correlation coefficient, \eqn{q^2}{q^2} in comparison to the well known conventional squared correlation coefficient, \eqn{r^2}{r^2}.
For a given model \var{M}, \eqn{q^2}{q^2} indicates the prediction performance of \var{M}, whereas \eqn{r^2}{r^2} is a measure for its calibration performance.
%The prediction performance of a model can be indicated with \eqn{q^2}{q^2}, whereas \eqn{r^2}{r^2} is a measure for the calibration performance of a model.
}
\details{
\tabular{ll}{
Package: \tab cvq2\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2012-12-03\cr
Depends: \tab stats\cr
License: \tab GPL v3\cr
LazyLoad: \tab yes\cr
}
%% FORMEL BESCHREIBEN
% y_fit: r^2 - DataSet + External TestSet, Vorhersagewerte aus N Beobachtungen DataSet, y_mean aus y(DataSet)
% y_pred: q^2 - DataSet + External TestSet, Vorhersagewerte aus N-1 Beobachtungen, exklusive der i-ten Beobachtung, jeder Wert aus TestSet wird N-mal vorhergesagt(?), y_mean ist das Gleiche wie fuer y_fit -> y(DataSet)
% y_pred: q^2_tr - DataSet + External TestSet, Vorhersagewerte aus N-1 Beobachtungen, exklusive der i-ten Beobachtung, y_mean fuer N-1 y-Werte aus dem Trainingsset 
% y_pred(N-k): q^2_cv - DataSet -> TrainingSet + TestSet - vorhergesagte Werte Testset, Parameter werden aus Trainingset generiert, y_mean fuer N-k y-Werte aus dem Trainingsset
%U+2261 kongruent \u2661, \u2263 - 4fach Gleichheitszeichen
The calculation procedure is as follows:\cr
For a data set, a general linear regression is performed to calculate the conventional squared correlation coefficient, \eqn{r^2}{r^2}:
\deqn{r^2 = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{fit} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}\right)^2} \equiv 1 - \frac{RSS}{SS}}{ q^2 = 1 - (SIGMA_i=1^N (y_i^fit - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean)^2) \u2261 1 - RSS/SS}
The observed values \eqn{y_i}{y_i} are compared to the fitted values \eqn{y_i^{fit}}{y_i^fit} calculated with the linear regression and yield to the calibration performance, \eqn{r^2}{r^2}, of the described model. 
The denominator complies with the \strong{R}esidual \strong{S}um of \strong{S}quares \emph{RSS}, the difference between the fitted values \eqn{y_i^{fit}}{y_i^fit} and the observed values \eqn{y_i}{y_i}.
The numerator is the \strong{S}um of \strong{S}quares, which are often called \emph{SS} in statistics, and refers to the difference between the observed values \eqn{y_i}{y_i} and their mean \eqn{y_{mean}}{y_mean}. 
To compare the calibration of the model with its prediction power, the model is applied to an external data set. 
The comparison of the predicted values \eqn{y_i^{pred}}{y_i^pred} with the observed values \eqn{y_i}{y_i} leads to the predictive squared correlation coefficient, \eqn{q^2}{q^2}:  
\deqn{q^2 = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}\right)^2} \equiv 1 - \frac{PRESS}{SS}}{ q^2 = 1 - (SIGMA_i=1^N (y_i^pred - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean)^2) \u2261 1 - PRESS/SS}
The \strong{PRE}dictive residual \strong{S}um of \strong{S}quares \emph{PRESS} is the difference between the predicted values \eqn{y_i^{pred}}{y_i^pred} and the observed values \eqn{y_i}{y_i}.
The \strong{S}um of \strong{S}quares \emph{SS} refers to the difference between the observed values \eqn{y_i}{y_i} and their mean \eqn{y_{mean}}{y_mean}.

%, not the arithemtic mean obtained from the observed values in the initial data set.
To avoid any bias, \eqn{y_{mean}}{y_mean} is the arithemtic mean of the \eqn{y_i}{y_i} from the external data set.
Hence the clarifying \eqn{q^2_{tr}}{q^2_tr} equation is slighlty different to the previous \eqn{q^2}{q^2} equation:
\deqn{q^2_{tr} = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}^{training}\right)^2} }{ q_tr^2 = 1 - (SIGMA_i=1^N (y_i^pred - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean^training)^2)}
The arithmetic mean of the observed values in the external data set, \eqn{y_{mean}^{training}}{y_mean^training}, is used to determine the prediction performance \eqn{q^2_{tr}}{q^2_tr} of this model.

Furthermore, if no external data set is available, one can perform a cross validation to evaluate the prediction performance.
The cross validation splits the data set (\eqn{N}{N} elements) into a training set (\eqn{N-k}{N-k} elements) and a test set (\eqn{k}{k} elements). 
Each training set yields to a model, which is used to predict the missing \eqn{k}{k} value(s).
At least, any observed value is predicted once and the comparison between the observation and the prediction yields to \eqn{q^2_{cv}}{q^2_cv}:
\deqn{q^2_{cv} = 1-\frac{\sum\limits_{i=1}^N\left( y_i^{pred(N-k)} - y_i\right)^2}{\sum\limits_{i=1}^N\left( y_i - y_{mean}^{N-k,i}\right)^2} }{ q_cv^2 = 1 - SIGMA_i=1^N (y_i^pred(N-k) - y_i)^2 / (SIGMA_i=1^N (y_i - y_mean^(N-k,i)^2} 
The arithmetic mean used in this equation, \eqn{y_{mean}^{N-k,i}}{y_mean^N-k,i}, is individually for any test set and calculated from the observed values comprised in the training set.

If \eqn{k > 1}{k>1}, the compilation of training and test set may have impact on the calculation of the predictive squared correlation coefficient.
To avoid biasing, one can repeat this calculation with various compilations of training and test set. 
Thus, any observed value is predicted multiple times, according to the number of runs performed.
% comprised == beinhalten, ginge auch contain, involve, imply, available from
Remark, in case of a cross validation, the calculation of the predictive squared correlation coefficient, \eqn{q^2}{q^2}, is more accurate than the calculation of the conventional squared correlation coefficient, \eqn{r^2}{r^2}.
}
\author{
Torsten Thalheim <torstenthalheim@gmx.de>
}
\references{
%%\inputenconding{utf8}
%%\usepackage[utf8]{inputenc}
\enumerate{
\item Cramer RD III. 1980. BC(DEF) Parameters. 2. An Empirical Structure-Based Scheme for the Prediction of Some Physical Properties. \emph{J. Am. Chem. Soc.} \bold{102:} 1849-1859.
\item Cramer RD III, Bunce JD, Patterson DE, Frank IE. 1988. Crossvalidation, Bootstrapping, and Partial Least Squares Compared with Multiple Linear Regression in Conventional QSAR Studies. \emph{Quant. Struct.-Act. Relat.} \bold{1988:} 18-25.
\item Organisation for Economic Co-operation and Development. 2007. Guidance document on the validation of (quantitative) structure-activity relationship [(Q)SAR] models. \emph{OECD Series on Testing and Assessment 69.} OECD Document ENV/JM/MONO(2007)2, pp 55 (paragraph no. 198) and 65 (Table 5.7).
\item \enc{Schüürmann}{Schuurmann} G, Ebert R-U, Chen J, Wang B, \enc{Kühne}{Kuhne} R. 2008. External validation and prediction employing the predictive squared correlation coefficient - test set activity mean vs training set activity mean. \emph{J. Chem. Inf. Model.} \bold{48:} 2140-2145.
\item Tropsha A, Gramatica P, Gombar VK. 2003. The Importance of Being Earnest: Validation is the Absolute Essential for Successful Application and Interpretation of QSPR Models. \emph{QSAR Comb. Sci.} \bold{22:} 69-77.
}
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
result <- cvq2( cvq2.setA, y ~ x1 + x2 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nGroup = 3 )
result

data(cvq2.setB)
result <- cvq2( cvq2.setB, y ~ x, nGroup = 3, nRun = 5 )
result
}