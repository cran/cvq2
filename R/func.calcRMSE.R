func.calcRMSE <-
function( yiCalc, yi, ysample=FALSE ){
  n <- NROW(yiCalc)
  if( ysample )
    decrement(n)

  return( sqrt( sum( (yiCalc - yi) * (yiCalc - yi) ) / n ) )
}

