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
  
  nFormat <- format(round(c(result$fit$observed_mean, result$fit$predicted_mean, result$fit$rmse, result$fit$r2), digits = output$round), nsmall = output$round)
  
  cat("mean (observed): \t", nFormat[1], "\n", sep="", file = output$writeTarget )
  cat("mean (predicted): \t", nFormat[2], "\n", sep="", file = output$writeTarget )
  cat("rmse: \t\t\t", nFormat[3], "\n", sep="", file = output$writeTarget )
  cat("r^2: \t\t\t", nFormat[4], "\n", sep="", file = output$writeTarget )
  
  out.perf = "model and prediction set available"
  if( !is.null(result$cv) )
    out.perf = "cross validation"
  cat( "\n-- PREDICTION PERFORMANCE (", out.perf, ")\n", sep="", file = output$writeTarget )
  
  if( !is.null(result$cv) ){
    #cross validation specific values
    cat("#Runs: \t\t\t\t", result$cv$nRun, "\n", sep="", file = output$writeTarget )
    cat("#Groups: \t\t\t",result$cv$nFold, "\n", sep="", file = output$writeTarget )
    
#    y_means der gruppen
  
    if( result$cv$decimalSplit ){
      varyTrainingSet = " (+1)"
      varyTestSet = " (-1)"
    }
  }
  
  out.train = "#Elements Model Set: \t\t"
  out.test = "#Elements Prediction Set: \t" 
  
  if( !is.null(result$cv) ){
    out.train = "#Elements Training Set: \t"
    out.test = "#Elements Test Set: \t\t"
  }

  cat( out.train, result$pred$nTrainingSet,varyTrainingSet, "\n",sep="", file = output$writeTarget )
  cat( out.test, result$pred$nTestSet,varyTestSet, "\n\n",sep="", file = output$writeTarget )

  nFormat <- format(round(c(result$pred$observed_mean, result$pred$predicted_mean, result$pred$rmse, result$pred$q2), digits = output$round), nsmall = output$round)

  cat("mean (observed): \t", nFormat[1], "\n", sep="", file = output$writeTarget )
  cat("mean (predicted): \t", nFormat[2], "\n", sep="", file = output$writeTarget )
  cat("rmse: \t\t\t", nFormat[3], "\n",sep="", file = output$writeTarget )
  # calculated with Y_mean^training
  cat("q^2: \t\t\t", nFormat[4], "\n",sep="", file = output$writeTarget )
}

