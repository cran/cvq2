func.crossValidationAnalysis <-
function( input, output ){
  tmp <- NULL
  samp <- NULL
  
  cv.datatable <- data.frame(
    cbind(
      "no" = numeric(0),
      "meanY.LXO" = numeric(0), 
      "obs" = numeric(0), 
      "pred" = numeric(0)
    )
  )

  # extend the dataframe for the regression values
  cv.datatable <- func.extendDataframeForRegValues( cv.datatable, output$coefficients )
  cv.datatable_columns <- matrix(
    c(paste( "C", 1:NCOL(cv.datatable), sep =""), letters[1:NCOL(cv.datatable)]),
    nrow = NCOL(cv.datatable),
    ncol = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )
  colnames(cv.datatable) <- cv.datatable_columns[,"abrev"]

  tmp$colnames <- c( "no in modelData", "Y_mean^training", "observed value", "predicted value" )

  cv.datatable_columns[1:NROW(tmp$colnames),"trans"] <- tmp$colnames
  tmp$colnames.i <- NROW(tmp$colnames) + 1

  if(names(output$coefficients)[1] == "(Intercept)"){
    cv.datatable_columns[tmp$colnames.i,"trans"] <- "const"
    increment(tmp$colnames.i)
  }
  
  for( i in tmp$colnames.i:NCOL(cv.datatable) )
    cv.datatable_columns[i,"trans"] <- letters[i - tmp$colnames.i + 1]

  tmp$cv$no_col <- cv.datatable_columns[cv.datatable_columns[,"trans"]=="no in modelData", "abrev"]
  tmp$cv$pred_col <- cv.datatable_columns[cv.datatable_columns[,"trans"]=="predicted value", "abrev"]
  tmp$cv$obs_col <- cv.datatable_columns[cv.datatable_columns[,"trans"]=="observed value", "abrev"]
  tmp$cv$meanY_col <- cv.datatable_columns[cv.datatable_columns[,"trans"]=="Y_mean^training", "abrev"]

  #perform cross validation
  samp$no = 1 
  input$sampleOrder <- NULL

  for( iRun in 1:input$nRun ){
    samp$orderThisRun <- func.createSample( input ) 
    input$runSampleOrder[iRun] <- list( samp$orderThisRun )
  
    while(NROW(samp$orderThisRun) > 0){
      if( NROW(samp$orderThisRun)%%input$nTestSet == 0)
        samp$rows <- samp$orderThisRun[c(1:input$nTestSet)]
      else
        samp$rows <- samp$orderThisRun[c(1:(input$nTestSet - 1))]
    
      samp$trainingSet <- input$modelData[-samp$rows,] #regressionSet
      samp$testSet <- input$modelData[samp$rows[order(samp$rows)],]  #predictionSet, ordered

      tmp$testSet[samp$no] <- list( samp$testSet )
  
      samp$model <- glm( formula = input$regressionFormula, data=samp$trainingSet )
      
      if(input$nRun == 1)
        tmp$rowNo <- rownames(samp$testSet)
      else
        tmp$rowNo <- 1:nrow(samp$testSet)+NROW(cv.datatable)

      cv.datatable[tmp$rowNo, tmp$cv$pred_col] <- predict(samp$model, samp$testSet)
      cv.datatable[tmp$rowNo, tmp$cv$obs_col] <- samp$testSet[, ncol(input$modelData)]
      cv.datatable[tmp$rowNo, tmp$cv$meanY_col] <- rep( mean(samp$trainingSet[, ncol(input$modelData)]), NROW(tmp$rowNo))
      cv.datatable[tmp$rowNo, tmp$cv$no_col] <- as.numeric(rownames(samp$testSet))
     
      for(c in 1:NROW(output$coefficients) )
        cv.datatable[tmp$rowNo, ncol(cv.datatable)-NROW(output$coefficients)+c] <- rep( samp$model$coefficients[c], NROW(tmp$rowNo))
                                                                                               
      samp$orderThisRun <- samp$orderThisRun[-c(1:NROW(samp$rows))]
  
      if( !is.null(output$writeTarget) ){
        cat("Sample",samp$no,":",sort(samp$rows),"\n", file = output$writeTarget)
        cat("Leave-X-Out Mean",mean(samp$trainingSet[,ncol(input$modelData)]),"\n", file = output$writeTarget)
        func.output.regressionFormulaWithCoefficients(output, colnames(input$modelData))

        writeLines("", con = output$writeTarget)
      }
      increment(samp$no)
    }
#    tmp$rmse[iRun] <- func.calcRMSE(cv.datatable[,tmp$cv$pred_col], cv.datatable[,tmp$cv$obs_col], TRUE)

    increment(iRun)
  }

  #copy it unsorted  
  pred.datatable <- cv.datatable
#  print(order(pred.datatable[tmp$cv$no_col,]))
  pred.datatable <- pred.datatable[order(pred.datatable[,tmp$cv$no_col]),]
  pred.datatable <- pred.datatable[ ,c(tmp$cv$no_col, tmp$cv$obs_col, tmp$cv$pred_col) ]

  # sort the cv.datatable in case of sample fractioning
#  pred.datatable <- pred.datatable[order(pred.datatable[tmp$cv$no_col,]),]
#  print(cv.datatable)
#  print(pred.datatable)

  tmp$pred$colnames <- c( "no in modelData", "observed value", "predicted value" )

  pred.datatable_columns <- matrix(                                                                                  
    c(paste( "C", 1:NCOL(pred.datatable), sep =""), letters[1:NCOL(pred.datatable)]),
    nrow = NCOL(pred.datatable),
    ncol = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )

  colnames(pred.datatable) <- pred.datatable_columns[,"abrev"]
  pred.datatable_columns[1:NROW(tmp$pred$colnames),"trans"] <- tmp$pred$colnames

  if( !is.null(output$writeTarget) ){
    writeLines("-- Start OVERVIEW Parameter Cross Validation --", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
    #use coefficients from glm, should be same for cross valdiation
    func.output.linearFormula( output )
    writeLines("", con = output$writeTarget)
  }
#  print(output$coefficients)

  if( !is.null(output$writeTarget) ){
    writeLines("Data Table: ", con = output$writeTarget)
    cat( cv.datatable_columns[,"abrev"], "\n", sep="\t", file = output$writeTarget )
  
    write.table( round(cv.datatable,output$round), file = output$writeTarget, sep="\t", row.names = FALSE, col.names = FALSE )
    writeLines("", con = output$writeTarget)
  
    writeLines("Data Table column names explanation:", con = output$writeTarget)
    write.table( cv.datatable_columns, file = output$writeTarget, sep="\t", row.names = FALSE, col.names=FALSE )
  
    writeLines("-- End OVERVIEW Parameter Cross Validation --", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
  }
  
  #hier koennten theoretisch noch die einzelnen samples und ihre ergebnisse rein
  tmp$res$cv = list(
    #redundant to res$pred
    "nTestSet" = input$nTestSet,
    "nTrainingSet" = input$nTrainingSet,
    #
    "nFold" = input$nFold,
    "decimalSplit" = input$decimalSplit,
    #rewrite this value(s)
    "nRun" = input$nRun,
    
    "datatable" = cv.datatable,
    "datatable_columns" = cv.datatable_columns,
    "TestSet" = tmp$testSet
  )
    
  tmp$res$pred = list(
    "nTestSet" = input$nTestSet,
    "nTrainingSet" = input$nTrainingSet,
    #at least the mean column must be from cv.datatable
    "q2" = func.calcXSquare(cv.datatable[,tmp$cv$pred_col], cv.datatable[,tmp$cv$obs_col], cv.datatable[,tmp$cv$meanY_col] ),
    # berechnet man den kompletten Datensatz mit einem Model -> N
    # berechnet man nur eine Stichprobe des Datensatzes mit einem Model -> n-1, cross validation ist Stichprobe, da nur x-Elemente vorhergesagt werden
    #one can use here pred.datatable as well
    # but because the headers tmp$cv$pred_col ... are defined for cv.datatable
    "rmse" = func.calcRMSE(cv.datatable[,tmp$cv$pred_col], cv.datatable[,tmp$cv$obs_col], TRUE),
    "observed_mean" = mean(cv.datatable[, tmp$cv$obs_col]),
    "predicted_mean" = mean(cv.datatable[, tmp$cv$pred_col]),
    "datatable" = pred.datatable,
    "datatable_columns" = pred.datatable_columns
  )

  #number of different test sets is missing
  return( tmp$res )
}

