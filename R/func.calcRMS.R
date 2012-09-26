func.calcRMS <-
function(yiCalc, yi, count){
  #return rms
  sqrt( sum( (yiCalc - yi) * (yiCalc - yi) ) / count)
}

