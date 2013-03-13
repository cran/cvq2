cvqsq <-
function( modelData, formula = NULL, nFold = N, nRun = 1,
round = 4, extOut = FALSE, extOutFile = NULL  ){
#  call <- match.call()
  N <- nrow(modelData)

  #forward to the actual method
  x <- mainfunc.q2( modelData, NULL, formula, nFold, nRun, round, extOut, extOutFile, match.call() )
  return(x)
}

