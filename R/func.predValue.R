func.predValue <-
function( model, pred, pos ){
  tmp <- NULL
  tmp$pred = 0
  tmp$start = 1
  
  if(names(model$coefficients)[1] == "(Intercept)"){
    tmp$pred = model$coefficients[1]
    increment(tmp$start)
  }

  for(i in tmp$start:NROW(model$coefficients))
    tmp$pred = tmp$pred + model$coefficients[i] * pred[pos, i - tmp$start + 1]

  return( tmp$pred )
}

