cvq2 <-
function( data, formula = NULL, round = 4, extOut = FALSE, extOutFile = NULL ){

  dataCV.input <- data  #to predict cross validation data
  regressionFormula <- formula
  output.round <- round
  output.filename <- extOutFile
  writeOutputTarget <- NULL
  output.toFile <- FALSE
  
  result <- NULL
  result$cv$repeated_runs <- 0

  #change DEFAULT
  if( !is.null(output.filename) )
    extOut <- TRUE

  #identify DEFAULT
  if( is.null(regressionFormula) )
    regressionFormula = func.constructRegressionFormula( colnames(dataCV.input) )

  if( extOut ){
    if( is.null(output.filename) )
      writeOutputTarget = stdout()
    #redirect output to file
    else{
      output.toFile <- TRUE
      writeOutputTarget = file(output.filename, open = "w")
    }
  }

  # exclude all lines with error
  dataCV.input = dataCV.input[rowSums(is.na(dataCV.input)) == 0,]

  #reorder the columns to match the given order in formula
  dataCV.input <- func.sortDataColumns( dataCV.input, regressionFormula, writeOutputTarget )

  #data.meanY
  result$r2$observed_mean = mean(dataCV.input[,ncol(dataCV.input)])

  if( !is.null(writeOutputTarget) ){
    writeLines("Data Set: ", con = writeOutputTarget)
    write.table( dataCV.input, file = writeOutputTarget, sep="\t", row.names = FALSE )
  #  writeLines("Y_mean: ", con = writeOutputTarget)
  #  write.table( round(result$r2$observed_mean, output.round), file = writeOutputTarget, sep="\t", row.names = FALSE )
    writeLines("", con = writeOutputTarget)
  }

  #
  # full regression
  #
  dataCV.fit <- glm(regressionFormula, data=dataCV.input)
  tmp.coeff <- dataCV.fit$coefficients

  if( !is.null(writeOutputTarget) ){
    writeLines("\n---- Start full regression ----", con = writeOutputTarget)
    func.output.regressionFormulaWithCoefficients(tmp.coeff, output.round, colnames(dataCV.input), writeOutputTarget)
    writeLines( "", con = writeOutputTarget )
  }
  
  dataCV.fit.pred <- 1:nrow(dataCV.input)

  for(i in 1:nrow(dataCV.input) )
    dataCV.fit.pred[i] <- func.predValue(dataCV.fit, dataCV.input, i)

  dataCV.fit.tablenames <- matrix(
    c(paste( "C", 1:2, sep =""), c("observed value", "predicted value") ),
    nrow = 2,
    ncol=2,
    byrow=FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )
  dataCV.fit.datatable <- cbind(dataCV.input[,NCOL(dataCV.input)], dataCV.fit.pred )
  colnames(dataCV.fit.datatable) <- dataCV.fit.tablenames[,"abrev"]

  if( !is.null(writeOutputTarget) ){
    writeLines("Observed vs. predicted values: ", con = writeOutputTarget)
    cat( dataCV.fit.tablenames[,"abrev"], "\n", sep="\t", file = writeOutputTarget )
    write.table( round(dataCV.fit.datatable,output.round), file = writeOutputTarget, sep="\t", row.names = FALSE, col.names = FALSE )
  
    writeLines("", con = writeOutputTarget)
    writeLines("Table column names explanation:", con = writeOutputTarget)
    write.table( dataCV.fit.tablenames, file = writeOutputTarget, sep="\t", row.names = FALSE, col.names=FALSE )
  }

  #calc r-square
  result$r2 <- list(
    "value" = func.calcXSquare(dataCV.fit.pred, dataCV.input[,ncol(dataCV.input)], result$r2$observed_mean),
    "rms" = func.calcRMS(dataCV.fit.pred, dataCV.input[,ncol(dataCV.input)],  nrow(dataCV.input) ),
    "elements" = nrow(dataCV.input),
    "datatable" = dataCV.fit.datatable,
    "column_names" = dataCV.fit.tablenames,
    #rewrite this value(s)
    observed_mean = result$r2$observed_mean
  )

  if( !is.null(writeOutputTarget) ){
    writeLines("---- End full regression ----", con = writeOutputTarget)
    writeLines("", con = writeOutputTarget)
  }
  #
  # leave-one-out, cross validation
  #
  # compareFrame for observed (experimental) and predicted data
  dataCV.expPred <- data.frame(cbind("meanY.LOO" = 1:nrow(dataCV.input), "obs" = dataCV.input[,ncol(dataCV.input)], "pred" = 1:nrow(dataCV.input)))

  # extend the dataframe for the regression values
  dataCV.expPred <- func.extendDataframeForRegValues(dataCV.expPred, dataCV.fit$coefficients, dataCV.input)

  for(i in 1:nrow(dataCV.input) ) {
    dataCV.expPred[i, 1] <- mean(dataCV.input[-i,ncol(dataCV.input)])
  #  dataCV.expPred[i, 1] <-  round ( (dataCV.YSum - dataCV.input[i,ncol(dataCV.input)]) / ( sum(!is.na(dataCV.fit.pred) ) -1 ) , 4 )
    dataCV.expPred <- func.leaveOneOut(i, dataCV.input, regressionFormula, dataCV.expPred, output.round)
  }

  #dataCV.expPred = dataCV.expPred[!is.na(dataCV.expPred[,"pred"]),]

  if( !is.null(writeOutputTarget) ){
    writeLines("---- Start leave-one-out cross validation ----", con = writeOutputTarget)
    writeLines("-- OVERVIEW Parameter Cross Validation --", con = writeOutputTarget)
    writeLines("", con = writeOutputTarget)
    #use coefficients from glm, should be same for cross valdiation
    func.output.linearFormula( tmp.coeff, writeOutputTarget)
    writeLines("", con = writeOutputTarget)
  }
#  print(tmp.coeff)

  dataCV.expPred.tablenames <- matrix(
    c(paste( "C", 1:NCOL(dataCV.expPred), sep =""), letters[1:NCOL(dataCV.expPred)]),
    nrow = NCOL(dataCV.expPred),
    ncol=2,
    byrow=FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )

  other.colnames <- c("Y_mean^training", "observed value", "predicted value")

  dataCV.expPred.tablenames[1:NROW(other.colnames),"trans"] <- other.colnames
  equation.colnames.i <- NROW(other.colnames) + 1

  if(names(tmp.coeff)[1] == "(Intercept)"){
    dataCV.expPred.tablenames[equation.colnames.i,"trans"] <- "const"
    increment(equation.colnames.i)
  }

  for( i in equation.colnames.i:NCOL(dataCV.expPred) ){
#    cat( i, equation.colnames.i, letters[i - equation.colnames.i + 1], "\n", sep=" ## ")
    dataCV.expPred.tablenames[i,"trans"] <- letters[i - equation.colnames.i + 1]
  }

  if( !is.null(writeOutputTarget) ){
    writeLines("Data Table: ", con = writeOutputTarget)
    cat( dataCV.expPred.tablenames[,"abrev"], "\n", sep="\t", file = writeOutputTarget )
  
    write.table( round(dataCV.expPred,output.round), file = writeOutputTarget, sep="\t", row.names = FALSE, col.names = FALSE )
    writeLines("", con = writeOutputTarget)
  
    writeLines("Data Table column names explanation:", con = writeOutputTarget)
    write.table( dataCV.expPred.tablenames, file = writeOutputTarget, sep="\t", row.names = FALSE, col.names=FALSE )
  }
  
  #number of different test sets is missing
  result$cv <- list(
    "datatable" = dataCV.expPred,
    "column_names" = dataCV.expPred.tablenames,
    "elements_test_set" = 1,
    "split_size_data_set" = 1,
    "splitted_sets_count" = result$r2$elements[1],
    #rewrite this value(s)
    "repeated_runs" = result$cv$repeated_runs
  )
  result$cv$elements_training_set <- result$r2$elements[1]-result$cv$elements_test_set[1]

  colnames(result$cv$datatable) <- dataCV.expPred.tablenames[,"abrev"]

#  result$q2 <- list(
#    "value" = func.calcXSquare(dataCV.expPred[,"pred"], dataCV.expPred[,"obs"], result$r2$observed_mean),
#    "rms" = func.calcRMS(dataCV.expPred[,"pred"], dataCV.expPred[,"obs"], NROW(dataCV.expPred) - 1)
#  )

  #different ways to perform -> see r2 list
  result$cv$q2 <- list(
    "value" = func.calcXSquare(dataCV.expPred[,"pred"], dataCV.expPred[,"obs"], dataCV.expPred[,"meanY.LOO"]),
    # n - 1, da Stichprobe
    "rms" = func.calcRMS(dataCV.expPred[,"pred"], dataCV.expPred[,"obs"], NROW(dataCV.expPred) - 1)
  )

  if( !is.null(writeOutputTarget) ){
    writeLines("---- End leave-one-out cross validation ----", con = writeOutputTarget)
    writeLines("", con = writeOutputTarget)
  }
#  print(result)

  # data output
  func.output.PerformanceValues(
    result,
    output.round,
    writeOutputTarget
  )

  if(output.toFile){ close(writeOutputTarget) }
  
#  print(result)
  result
}

