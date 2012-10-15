func.constructRegressionFormula <-
function( col.name ){
  col.formula = ""

  for( i in NROW(col.name):1 ){
    tmp.sign = "+"
    if( i == NROW(col.name) )
      tmp.sign = ""
    if( i == NROW(col.name)-1 )
      tmp.sign = "~"

    col.formula = paste( col.formula, tmp.sign, col.name[i] )
  }
  as.formula(col.formula)
}

