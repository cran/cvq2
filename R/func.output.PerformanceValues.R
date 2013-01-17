func.output.performanceValues <-
function(result, output){
#TODO later: use format 

  varyTrainingSet = ""
  varyTestSet = ""

  writeLines("\n---- CALL ----", con = output$writeTarget)
  writeLines(deparse(output$call), con = output$writeTarget)

  writeLines("\n---- RESULTS ----\n", con = output$writeTarget)

  writeLines( "-- MODEL CALIBRATION (linear regression)", con = output$writeTarget )
  cat( "#Elements: \t",result$fit$n,"\n\n",sep="", file = output$writeTarget )
  
  #to avoid 9 -> 1 digit, 10 -> 2 digits -> log10 == 1
  nDigits <- floor(log10(max(result$fit$rmse, result$fit$r2))) + 1

  format(c(6.0, 13.5), digits = 2, nsmall = 0)

  cat("rmse: \t\t", format(result$fit$rmse, digits = 2, nsmall = output$round), "\n", sep="", file = output$writeTarget )
  # (use Y_mean)
  cat("r^2: \t\t", format(result$fit$r2, digits = 2, nsmall = output$round), "\n", sep="", file = output$writeTarget )

  writeLines( "\n-- PREDICTION PERFORMANCE (cross validation)", con = output$writeTarget )

  cat("#Runs: \t\t\t\t", result$cv$nRun, "\n", sep="", file = output$writeTarget )
  cat("#Groups: \t\t\t",result$cv$nGroup, "\n", sep="", file = output$writeTarget )

  if( result$cv$decimalSplit ){
    varyTrainingSet = " (+1)"
    varyTestSet = " (-1)"
  }

  cat("#Elements Training Set: \t",result$cv$nTrainingSet,varyTrainingSet, "\n",sep="", file = output$writeTarget )
  cat("#Elements Test Set: \t\t",result$cv$nTestSet,varyTestSet, "\n\n",sep="", file = output$writeTarget )

  cat("rmse: \t\t\t\t", round(result$cv$rmse, output$round), "\n",sep="", file = output$writeTarget )
  # calculated with Y_mean^training
  cat("q^2_cv : \t\t\t", round(result$cv$q2, output$round), "\n",sep="", file = output$writeTarget )
}

