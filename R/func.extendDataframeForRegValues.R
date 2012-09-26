func.extendDataframeForRegValues <-
function(extFrame, coeff, cvPre){
  if(names(coeff)[1] == "(Intercept)"){
    for(i in 1:ncol(cvPre)){
      extFrame[, i+3] <- 0
      colnames(extFrame)[i+3] <- ifelse(i == 1, "const", letters[i-1] )
    }
  }else{
    for(i in 1:(ncol(cvPre)-1)){
      extFrame[, i+3] <- 0
      colnames(extFrame)[i+3] <- letters[i]
    }
  }
  
  extFrame
}

