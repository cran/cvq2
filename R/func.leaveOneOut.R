func.leaveOneOut <-
function(leaveOut, data, formula, regOut, output.round ){
  data.fit <- glm(formula, data=data[-leaveOut,])

  for(c in 1:NROW(data.fit$coefficients) )
    regOut[leaveOut, 3+c] = data.fit$coefficients[c]

  pred = func.predValue(data.fit, data, leaveOut)

  #Erwartungswert speichern
  regOut[leaveOut, 3] <- pred

  regOut
}

