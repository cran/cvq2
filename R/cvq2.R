cvq2 <-
function( data, formula = NULL, nGroup = N, nRun = 1,
round = 4, extOut = FALSE, extOutFile = NULL  ){
#  call <- match.call()
  N <- nrow(data)
  
  input <- list(
    "dataCV" = data,
    "regressionFormula" = formula,
    "nGroup" = nGroup,
    "nRun" = nRun
  )

  output <- list(
    "call" = match.call(),
    "round" = round,
    "toFile" = FALSE,
    "writeTarget" = NULL,
    "coefficients" = NULL
  )
  
  result <- NULL
  tmp <- NULL
  
#    input$splitSizeDataSet = floor( nrow(data) / input$nGroup )
  input$nTestSet = ceiling( nrow(data) / input$nGroup )
  input$nTrainingSetSet = nrow(data) - input$nTestSet
  # everytime the same size for training and test set
  input$variableSplit = FALSE 
  
  nTrainingSetMin = input$nTrainingSet
  
  # no equal distribution size for test and training set possible
  # distribution can vary (test set + 1), (training set - 1)
  if( input$nTestSet != NROW(data) / input$nGroup ){
    input$variableSplit = TRUE
    decrement(nTrainingSetMin)
  }
    
  # maximum N groups == Leave one out cross validation
  # there can not be more groups than elements in the data set
  if( input$nGroup > nrow(data) ){
    cat("It is not possible, to have more groups (",input$nGroup,") than elements in the data set exist (",nrow(data),").\n")
    stop("Change parameter settings and start againg")
  }

  # number of x + 1 is minimum for training set
  # here ncol == (number of x , y)
  if( input$nTrainingSet < ncol(data) ){
    cat("min(nTestSet) (",nTrainingSetMin,") is to small to create a linear model, must be at least (",ncol(data),")\n")
    stop("Change parameter settings and start againg")
  }
  
  #change DEFAULT
  if( !is.null(extOutFile) )
    extOut <- TRUE

  #identify DEFAULT
  if( is.null(input$regressionFormula) )
    input$regressionFormula = func.constructRegressionFormula( colnames(input$dataCV) )

  if( extOut ){
    if( is.null(extOutFile) )
      output$writeTarget = stdout()
    #redirect output to file
    else{
      output$toFile <- TRUE
      output$writeTarget = file(extOutFile, open = "w")
    }
  }

  # exclude all lines with error
  input$dataCV <- input$dataCV[rowSums(is.na(input$dataCV)) == 0,]

  #reorder the columns to match the given order in formula
  input$dataCV <- func.sortDataColumns( input, output$writeTarget )

  if( !is.null(output$writeTarget) ){
    writeLines("Data Set: ", con = output$writeTarget)
    write.table( input$dataCV, file = output$writeTarget, sep="\t", row.names = FALSE )
  #  writeLines("Y_mean: ", con = output$writeTarget)
  #  write.table( round(result$fit$observed_mean, output$round), file = output$writeTarget, sep="\t", row.names = FALSE )
    writeLines("", con = output$writeTarget)
  }

  # linear regression
  result$fit <- func.linearRegressionAnalysis( input, output )
#  print(result$fit)

  # leave-X-out, cross validation
  result$cv <- func.crossValidationAnalysis( input, output )
  
  # data output
  if( !is.null(output$writeTarget) )
    func.output.PerformanceValues( result, output )

  if( output$toFile )
    close(output$writeTarget)
  
#  print(result)
  return( result )
}

