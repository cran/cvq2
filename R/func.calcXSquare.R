func.calcXSquare <-
function(yiCalc, yi, yMean){
#  return xsquare <-
    1 -
    sum( (yiCalc - yi) * (yiCalc - yi)) /
    sum( (yi - yMean) * (yi - yMean) )
}

