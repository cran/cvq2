func.output.linearFormula <-
function(coeff, writeOutputTarget){
  if(names(coeff)[1] == "(Intercept)"){
    writeLines( paste("General Coefficients: Y = a*X1 + b*X2 + ... + const") , con = writeOutputTarget )
  }else{
    writeLines( paste("General Coefficients: Y = a*X1 + b*X2 + ... z*Xn"), con = writeOutputTarget )
  }
}

