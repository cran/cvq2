func.predValue <-
function(data.fit, data, pos, writeOutputTarget ){
  if(names(data.fit$coefficients)[1] == "(Intercept)"){
    pred = data.fit$coefficients[1]

    #writeLines(pred, con = writeOutputTarget )

    for(i in 2:NROW(data.fit$coefficients)){
      pred = pred + data.fit$coefficients[i] * data[pos, i - 1]
    }
  }else{
    pred = 0;
    for(i in 1:NROW(data.fit$coefficients)){
      pred = pred + data.fit$coefficients[i] * data[pos, i]
    }
  }

  pred
}

