func.linearRegressionAnalysis <-
function( input, output ){
  tmp <- NULL

  #data.meanY
  observed_mean = mean(input$dataCV[,ncol(input$dataCV)])

  model <- glm( formula = input$regressionFormula, data = input$dataCV )
  #replace the coefficients outside this function too
  eval.parent(substitute(output$coefficients <- model$coefficients))

  if( !is.null(output$writeTarget) ){
    writeLines("\n---- Start linear regression ----", con = output$writeTarget)
    func.output.regressionFormulaWithCoefficients( output, colnames(input$dataCV) ) 
    writeLines( "", con = output$writeTarget )
  }
  
  datatable_columns <- matrix(
    c(paste( "C", 1:2, sep =""), c("observed value", "predicted value") ),
    nrow = 2,
    ncol = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("abrev", "trans"))
  )
  
  datatable <- cbind( input$dataCV[,NCOL(input$dataCV)], 1:nrow(input$dataCV) )
  colnames(datatable) <- datatable_columns[,"abrev"]

  tmp$pred_col <- datatable_columns[datatable_columns[,"trans"]=="predicted value", "abrev"]
  
  for(i in 1:nrow(input$dataCV) )
    datatable[i, tmp$pred_col] <- func.predValue( model, input$dataCV, i )

  if( !is.null(output$writeTarget) ){
    writeLines("Observed vs. predicted values: ", con = output$writeTarget)
    cat( datatable_columns[,"abrev"], "\n", sep="\t", file = output$writeTarget )
    write.table( round(datatable,output$round), file = output$writeTarget, sep="\t", row.names = FALSE, col.names = FALSE )
  
    writeLines("", con = output$writeTarget)
    writeLines("Table column names explanation:", con = output$writeTarget)
    write.table( datatable_columns, file = output$writeTarget, sep="\t", row.names = FALSE, col.names=FALSE )

    writeLines("---- End linear regression ----", con = output$writeTarget)
    writeLines("", con = output$writeTarget)
  }
  
  return( 
    list(
      "r2" = func.calcXSquare( datatable[, tmp$pred_col], input$dataCV[,ncol(input$dataCV)], observed_mean ),
      "rmse" = func.calcRMSE( datatable[, tmp$pred_col], input$dataCV[,ncol(input$dataCV)] ),
      "n" = nrow(input$dataCV),
      "datatable_columns" = datatable_columns,
      "datatable" = datatable,
      "observed_mean" = observed_mean,
      "model" = model
    )      
  )
}

