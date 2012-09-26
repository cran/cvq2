func.output.coefficients <-
function(coeff, output.round, writeOutputTarget){
  func.output.linearFormula( coeff, writeOutputTarget)
  
  # output coefficents, formatted
  # NROW gross, da Vektor
  for(i in 1:NROW(coeff) ){
    value <- round(coeff[i], output.round)
    
    if(names(coeff)[1] == "(Intercept)"){
      writeLines( paste(ifelse(i == 1, "const: ", letters[i-1]), ": ", value, sep=""), con = writeOutputTarget )
    }else{
      writeLines( paste(letters[i], ": ", value, sep=""), con = writeOutputTarget )
    }
  }
}

