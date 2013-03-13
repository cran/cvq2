func.externalValidationAnalysis <-
function( input, output, result ){
  tmp <- NULL

  datatable <- data.frame(
    cbind(
#      "meanY.LXO" = numeric(0), 
      "obs" = numeric(0), 
      "pred" = numeric(0)
    )
  )

  tmp$colnames <- c( "observed value", "predicted value" )
  datatable_columns <- matrix(                                                                                  
    c(paste( "C", 1:NCOL(datatable), sep =""), letters[1:NCOL(datatable)]),
    nrow = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )

  colnames(datatable) <- datatable_columns[,"abrev"]
  datatable_columns[1:NROW(tmp$colnames),"trans"] <- tmp$colnames

  tmp$pred$obs_col <- datatable_columns[datatable_columns[,"trans"]=="observed value", "abrev"]
  tmp$pred$pred_col <- datatable_columns[datatable_columns[,"trans"]=="predicted value", "abrev"]
  tmp$pred$rows <- 1:NROW(input$predictData)
  tmp$pred$observed_mean <- mean(input$predictData[, ncol(input$predictData)])
  tmp$model$observed_mean <- mean(input$modelData[, ncol(input$modelData)])
  
#  print( result$fit$model )
  datatable[tmp$pred$rows, tmp$pred$pred_col] <- predict( result$fit$model, input$predictData )
  datatable[tmp$pred$rows, tmp$pred$obs_col] <- input$predictData[, ncol(input$predictData)]
  
  if( !is.null(output$writeTarget) ){
    writeLines("-- Start PARAMETER Prediction External Data Set --", con = output$writeTarget)
    writeLines("Data Table: ", con = output$writeTarget)
    cat( datatable_columns[,"abrev"], "\n", sep="\t", file = output$writeTarget )
  
    write.table( round(datatable,output$round), file = output$writeTarget, sep="\t", row.names = FALSE, col.names = FALSE )
    writeLines("", con = output$writeTarget)
  
    writeLines("Data Table column names explanation:", con = output$writeTarget)
    write.table( datatable_columns, file = output$writeTarget, sep="\t", row.names = FALSE, col.names=FALSE )
  
    writeLines("-- End PARAMETER Prediction External Data Set --", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
  }
    
  #number of different test sets is missing
  return(
    list(
      "nTestSet" = input$nTestSet,
      "nTrainingSet" = input$nTrainingSet,
      "q2" = func.calcXSquare(datatable[,tmp$pred$pred_col], datatable[,tmp$pred$obs_col], tmp$model$observed_mean ),
      # berechnet man den kompletten Datensatz mit einem Model -> N
      # berechnet man nur eine Stichprobe des Datensatzes mit einem Model -> n-1, cross validation ist Stichprobe, da nur x-Elemente vorhergesagt werden
      "rmse" = func.calcRMSE(datatable[,tmp$pred$pred_col], datatable[,tmp$pred$obs_col], TRUE),
      "observed_mean" = tmp$pred$observed_mean,
      "predicted_mean" = mean(datatable[, tmp$pred$pred_col]),
      #rewrite this value(s)
      "datatable" = datatable,
      "datatable_columns" = datatable_columns,
      "TestSet" = input$predictData
    )
  )
}

