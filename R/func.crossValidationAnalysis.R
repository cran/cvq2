func.crossValidationAnalysis <-
function( input, output ){
  tmp <- NULL
  samp <- NULL
  
  datatable <- data.frame(
    cbind(
      "meanY.LXO" = numeric(0), 
      "obs" = numeric(0), 
      "pred" = numeric(0)
    )
  )

  # extend the dataframe for the regression values
  datatable <- func.extendDataframeForRegValues( datatable, output$coefficients )

  datatable_columns <- matrix(
    c(paste( "C", 1:NCOL(datatable), sep =""), letters[1:NCOL(datatable)]),
    nrow = NCOL(datatable),
    ncol = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )
  colnames(datatable) <- datatable_columns[,"abrev"]

  tmp$colnames <- c("Y_mean^training", "observed value", "predicted value")

  datatable_columns[1:NROW(tmp$colnames),"trans"] <- tmp$colnames
  tmp$colnames.i <- NROW(tmp$colnames) + 1

  if(names(output$coefficients)[1] == "(Intercept)"){
    datatable_columns[tmp$colnames.i,"trans"] <- "const"
    increment(tmp$colnames.i)
  }
  
  for( i in tmp$colnames.i:NCOL(datatable) )
    datatable_columns[i,"trans"] <- letters[i - tmp$colnames.i + 1]

  tmp$cv$pred_col <- datatable_columns[datatable_columns[,"trans"]=="predicted value", "abrev"]
  tmp$cv$obs_col <- datatable_columns[datatable_columns[,"trans"]=="observed value", "abrev"]
  tmp$cv$meanY_col <- datatable_columns[datatable_columns[,"trans"]=="Y_mean^training", "abrev"]

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
    
      samp$trainingSet <- input$dataCV[-samp$rows,] #regressionSet
      samp$testSet <- input$dataCV[samp$rows[order(samp$rows)],]  #predictionSet, ordered

      tmp$testSet[samp$no] <- list( samp$testSet)
  
      samp$model <- glm(input$regressionFormula, data=samp$trainingSet)

      for(i in 1:nrow(samp$testSet) ) {
        if(input$nRun == 1)
          tmp$rowNo <- rownames(samp$testSet)[i]
        else
          tmp$rowNo <- NROW(datatable)+1
        
        #last column is everytime the value to predict -> sort done earlier ensures this
        datatable[tmp$rowNo, tmp$cv$meanY_col] <- mean(samp$trainingSet[, ncol(input$dataCV)])
        datatable[tmp$rowNo, tmp$cv$pred_col] <- func.predValue(samp$model, samp$testSet, i)
        datatable[tmp$rowNo, tmp$cv$obs_col] <- samp$testSet[i, ncol(input$dataCV)]
        
        for(c in 1:NROW(output$coefficients) )
          datatable[tmp$rowNo, ncol(datatable)-NROW(output$coefficients)+c] = samp$model$coefficients[c]
      }
      samp$orderThisRun <- samp$orderThisRun[-c(1:NROW(samp$rows))]
  
      if( !is.null(output$writeTarget) ){
        cat("Sample",samp$no,":",sort(samp$rows),"\n")
        cat("Leave-X-Out Mean",mean(samp$trainingSet[,ncol(input$dataCV)]),"\n", file = output$writeTarget)
        print(samp$model, file = output$writeTarget)
        writeLines("", con = output$writeTarget)
      }
      increment(samp$no)
    }
#    tmp$rmse[iRun] <- func.calcRMSE(datatable[,tmp$cv$pred_col], datatable[,tmp$cv$obs_col], TRUE)

    increment(iRun)
  }
  
  # sort the datatable in case of sample fractioning
  datatable <- datatable[order(as.numeric(rownames(datatable))),]
#  print(datatable)

  if( !is.null(output$writeTarget) ){
    writeLines("---- Start Leave-X-Out cross validation ----", con = output$writeTarget)
    writeLines("-- OVERVIEW Parameter Cross Validation --", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
    #use coefficients from glm, should be same for cross valdiation
    func.output.linearFormula( output )
    writeLines("", con = output$writeTarget)
  }
#  print(output$coefficients)

  if( !is.null(output$writeTarget) ){
    writeLines("Data Table: ", con = output$writeTarget)
    cat( datatable_columns[,"abrev"], "\n", sep="\t", file = output$writeTarget )
  
    write.table( round(datatable,output$round), file = output$writeTarget, sep="\t", row.names = FALSE, col.names = FALSE )
    writeLines("", con = output$writeTarget)
  
    writeLines("Data Table column names explanation:", con = output$writeTarget)
    write.table( datatable_columns, file = output$writeTarget, sep="\t", row.names = FALSE, col.names=FALSE )
  
    writeLines("---- End Leave-X-Out cross validation ----", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
  }
    
  #number of different test sets is missing
  return(
    list(
      "nTestSet" = input$nTestSet,
      "nTrainingSet" = input$nTrainingSet,
      "nGroup" = input$nGroup,
      "variableSplit" = input$variableSplit,
      "q2" = func.calcXSquare(datatable[,tmp$cv$pred_col], datatable[,tmp$cv$obs_col], datatable[,tmp$cv$meanY_col]),
      # berechnet man den kompletten Datensatz mit einem Model -> N
      # berechnet man nur eine Stichprobe des Datensatzes mit einem Model -> n-1, cross validation ist Stichprobe, da nur x-Elemente vorhergesagt werden
      "rmse" = func.calcRMSE(datatable[,tmp$cv$pred_col], datatable[,tmp$cv$obs_col], TRUE),
#      "rmse" = mean(tmp$rmse),
      #rewrite this value(s)
      "datatable" = datatable,
      "datatable_columns" = datatable_columns,
      "nRun" = input$nRun,
      "TestSet" = tmp$testSet
    )
  )
}

