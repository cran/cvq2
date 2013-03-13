looq2 <-
function( modelData, formula = NULL, round = 4, extOut = FALSE, extOutFile = NULL  ){
#  call <- match.call()
  N <- nrow(modelData)

  #forward to the actual method
  x <- mainfunc.q2( modelData, NULL, formula, N, 1, round, extOut, extOutFile, match.call() )
  return(x)
}

