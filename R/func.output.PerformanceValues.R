func.output.PerformanceValues <-
function(result, output){
#TODO later: use format 

  varyTrainingSet = ""
  varyTestSet = ""

  writeLines("\n---- CALL ----", con = output$writeTarget)
  writeLines(deparse(output$call), con = output$writeTarget)

  writeLines("\n---- RESULTS ----\n", con = output$writeTarget)

  writeLines( "-- LINEAR REGRESSION", con = output$writeTarget )
  cat( "#Elements: \t\t",result$fit$n,"\n\n",sep="", file = output$writeTarget )
  
  #to avoid 9 -> 1 digit, 10 -> 2 digits -> log10 == 1
  nDigits <- floor(log10(max(result$fit$rmse, result$fit$r2))) + 1

  format(c(6.0, 13.5), digits = 2, nsmall = 0)

  cat("rmse: \t\t\t", format(result$fit$rmse, digits = 2, nsmall = output$round), "\n", sep="", file = output$writeTarget )
  cat("r^2 (use Y_mean): \t", format(result$fit$r2, digits = 2, nsmall = output$round), "\n", sep="", file = output$writeTarget )

  writeLines( "-- CROSS VALIDATION", con = output$writeTarget )

  cat("#Runs: \t\t\t\t", result$cv$nRun, "\n", sep="", file = output$writeTarget )
  cat("#Groups: \t\t\t",result$cv$nGroup, "\n", sep="", file = output$writeTarget )

  if( result$cv$variableSplit ){
    varyTrainingSet = " (+1)"
    varyTestSet = " (-1)"
  }

  cat("#Elements Training Set: \t",result$cv$nTrainingSet,varyTrainingSet, "\n",sep="", file = output$writeTarget )
  cat("#Elements Test Set: \t\t",result$cv$nTestSet,varyTestSet, "\n\n",sep="", file = output$writeTarget )

  cat("rmse: \t\t\t\t", round(result$cv$rmse, output$round), "\n",sep="", file = output$writeTarget )
  cat("q^2_cv (use Y_mean^training): \t", round(result$cv$q2, output$round), "\n",sep="", file = output$writeTarget )
}

