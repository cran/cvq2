func.output.regressionFormulaWithCoefficients <-
function(coeff, output.round, parameter, writeOutputTarget){
  if( is.null(writeOutputTarget) )
    return()

  writeLines( "Regression formula with fitted parameters: ", con = writeOutputTarget)

  coeff.start = 1
  param.i = 1
  coeff.text <- NULL
  
  if(names(coeff)[1] == "(Intercept)")
    increment(coeff.start)

  for(i in coeff.start:NROW(coeff) ){
    coeff.text[NROW(coeff.text)+1] <- paste(round(coeff[[i]], output.round),"*",parameter[param.i])
    increment(param.i)
  }
  
  if( coeff.start == 2 )
    coeff.text[NROW(coeff.text)+1] <- round(coeff[[1]], output.round)
  
  #use collapse instead sep, to combine the coeff.text vector
  cat(parameter[param.i],"=", paste(coeff.text,collapse=" + "), "\n", file = writeOutputTarget)
}

