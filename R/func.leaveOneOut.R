func.leaveOneOut <-
function(leaveOut, data, formula, regOut, output.round, writeOutputTarget ){
  data.fit <- glm(formula, data=data[-leaveOut,])
  #writeLines(data.fit, con = writeOutputTarget )

#  writeLines( paste("Leave One Out: #",round(leaveOut,output.round),"(",round(data[leaveOut,ncol(data)],output.round),")"), con = writeOutputTarget )      # show results formatted
#  func.output.coefficients(data.fit$coefficients, output.round, writeOutputTarget)

  for(c in 1:NROW(data.fit$coefficients) )
    regOut[leaveOut, 3+c] = data.fit$coefficients[c]

  pred = func.predValue(data.fit, data, leaveOut, writeOutputTarget)
  
#  writeLines(paste("pred( ",round(pred,output.round), " ) vs exp( ", round(data[leaveOut,ncol(data)],output.round), " )\n", sep=""), con = writeOutputTarget )

  #Erwartungswert speichern
  regOut[leaveOut, 3] <- pred
  
  regOut
}

