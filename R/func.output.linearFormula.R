func.output.linearFormula <-
function(coeff, writeOutputTarget){
  if( is.null(writeOutputTarget) )
    return()

  last.coeff <- "z*Xn"
  if(names(coeff)[1] == "(Intercept)")
    last.coeff <- "const"

  writeLines( paste("General Equation: Y = a*X1 + b*X2 + ... + ", last.coeff) , con = writeOutputTarget )
}

