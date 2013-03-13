q2 <-
function( modelData, predictData, formula = NULL, round = 4, extOut = FALSE, extOutFile = NULL ){
#  call <- match.call()
  N <- nrow(predictData)

  #forward to the actual method
  x <- mainfunc.q2( modelData, predictData, formula, N, 1, round, extOut, extOutFile, match.call() )
  return(x)
}

