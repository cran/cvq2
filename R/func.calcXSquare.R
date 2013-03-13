func.calcXSquare <-
function( yiCalc, yi, yMean ){
#print("xsq")
#print(yiCalc)
#print(yi)
#print(yMean)

  return( 
    1 -
    sum( (yiCalc - yi) * (yiCalc - yi) ) /
    sum( (yi - yMean) * (yi - yMean) )
  )
}

