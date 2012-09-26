cvq2 <-
function( data, formula, round = 4, outFile = NULL ){

  dataCV.input <- data  #to predict cross validation data
  regressionFormula <- formula
  output.round <- round
  output.filename <- outFile
  
  writeOutputTarget = stdout()
  output.toFile <- FALSE
  
  #redirect output to file
  if( !is.null(output.filename) ){ 
    output.toFile <- TRUE
    writeOutputTarget = file(output.filename, open = "w")
  }
  
  # exclude all lines with error
  dataCV.input = dataCV.input[rowSums(is.na(dataCV.input)) == 0,]
  
  #reorder the columns to match the given order in formula
  dataCV.input <- func.sortDataColumns( dataCV.input, regressionFormula, writeOutputTarget )
  
  data.meanY = mean(dataCV.input[,ncol(dataCV.input)])
  
  writeLines("Dataset: ", con = writeOutputTarget)
  write.table( dataCV.input, file = writeOutputTarget, sep="\t", row.names = FALSE )
#  writeLines("mean(Y): ", con = writeOutputTarget)
#  write.table( round(data.meanY, output.round), file = writeOutputTarget, sep="\t", row.names = FALSE )
  writeLines("", con = writeOutputTarget)
  
  attach(dataCV.input)
  
  #
  # full regression
  #
  writeLines("\n---- Start full regression ----", con = writeOutputTarget)
  
  dataCV.fit <- glm(regressionFormula, data=dataCV.input)
  tmp.coeff <- dataCV.fit$coefficients
  
  func.output.coefficients(tmp.coeff, output.round, writeOutputTarget)
  writeLines( "", con = writeOutputTarget )
  dataCV.fit.pred <- 1:nrow(dataCV.input)
  
  for(i in 1:nrow(dataCV.input) )
    dataCV.fit.pred[i] <- func.predValue(dataCV.fit, dataCV.input, i, writeOutputTarget)
  
  writeLines("Predicted values: ", con = writeOutputTarget)
  write.table( round(dataCV.fit.pred,output.round), file = writeOutputTarget, sep="\t", row.names = FALSE )
  
  #calc r-square
  dataCV.fit.rsquare <- func.calcXSquare(dataCV.fit.pred, dataCV.input[,ncol(dataCV.input)], data.meanY)
  dataCV.fit.rms <- func.calcRMS(dataCV.fit.pred, dataCV.input[,ncol(dataCV.input)],  nrow(dataCV.input) )
  writeLines("---- End full regression ----", con = writeOutputTarget)
  writeLines("", con = writeOutputTarget)
                                        
  #
  # leave-one-out, cross validation
  #
  
  writeLines("---- Start leave-one-out cross validation ----", con = writeOutputTarget)
  
  # compareFrame for expm and pred data
  dataCV.expPred <- data.frame(cbind("meanY.LOO" = 1:nrow(dataCV.input), "expm" = dataCV.input[,ncol(dataCV.input)], "pred" = 1:nrow(dataCV.input)))
  
  # extend the dataframe for the regression values
  dataCV.expPred <- func.extendDataframeForRegValues(dataCV.expPred, dataCV.fit$coefficients, dataCV.input)
  
  for(i in 1:nrow(dataCV.input) ) {
    dataCV.expPred[i, 1] <- mean(dataCV.input[-i,ncol(dataCV.input)])
  #  dataCV.expPred[i, 1] <-  round ( (dataCV.YSum - dataCV.input[i,ncol(dataCV.input)]) / ( sum(!is.na(dataCV.fit.pred) ) -1 ) , 4 )
    dataCV.expPred <- func.leaveOneOut(i, dataCV.input, regressionFormula, dataCV.expPred, output.round, writeOutputTarget)
  }
  
  #dataCV.expPred = dataCV.expPred[!is.na(dataCV.expPred[,"pred"]),]
  
  writeLines("-- OVERVIEW Parameter Leave-one-out, mean(Y)_LOO used, experimental and predicted value, linear regression parameters(const, a...z) --", con = writeOutputTarget)
  writeLines("", con = writeOutputTarget)
  #use coefficients from glm, should be same for cross valdiation
  func.output.linearFormula( tmp.coeff, writeOutputTarget)

#  print(tmp.coeff)

  writeLines( paste( "mean(Y)_LOO", "observed value", "predicted value", "linear regression parameter (a,b,c...const)", sep="\t "), con = writeOutputTarget )
#  cat( "mean(Y)_LOO", "experimental value", "predicted value", "linear regression parameter", sep="\t", con = writeOutputTarget )
  write.table( round(dataCV.expPred,output.round), file = writeOutputTarget, sep="\t", row.names = FALSE, col.names = FALSE )
  
  attach(dataCV.expPred)
  
  dataCV.expPred.qSquare <- func.calcXSquare(pred, expm, data.meanY)
  dataCV.expPred.qSquareSubset <- func.calcXSquare(pred, expm, meanY.LOO)
  # n - 1, da Stichprobe
  dataCV.expPred.rms <- func.calcRMS(pred, expm, NROW(expm) - 1)
  writeLines("---- End leave-one-out cross validation ----", con = writeOutputTarget)
  writeLines("", con = writeOutputTarget)
  
  # data output
  func.output.PerformanceValues(
    #full regression
    dataCV.fit.rms,           # rms (full regression)
    NROW(expm),           # number of elements
    dataCV.fit.rsquare,   # r^2 result
    # cross validation
    1,
    0,
    dataCV.expPred.qSquare,    # q^2
    dataCV.expPred.qSquareSubset, # q^2 (Subset)
    dataCV.expPred.rms,          # rms(CV)
    NROW(expm) - 1,        # number of elements
    output.round,
    writeOutputTarget
  )
  
  # Detach only, if you want to start a new calculation with other values!
  # otherwise, detach after plot
  detach(dataCV.input)
  detach(dataCV.expPred)
  
  if(output.toFile){ close(writeOutputTarget) }
}

