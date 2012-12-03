func.sortDataColumns <-
function( input, writeOutputTarget){
  new.data <- cbind(input$dataCV[,all.vars(input$regressionFormula)[2]])
  colnames(new.data) <- all.vars(input$regressionFormula)[2]

  if( NCOL(input$dataCV) > 2 ){
    for( i in 3:NCOL(input$dataCV)){
      new.data <- cbind(new.data, input$dataCV[,all.vars(input$regressionFormula)[i]] )
      colnames(new.data)[i-1] <- all.vars(input$regressionFormula)[i]
    }
  }
#  colnames(new.data) <- all.vars(input$regressionFormula)

  # y as last column
  new.data <- cbind(new.data, input$dataCV[,all.vars(input$regressionFormula)[1]])
  colnames(new.data)[NCOL(input$dataCV)] <- all.vars(input$regressionFormula)[1]

  rownames(new.data) <- 1:NROW(new.data)
#  writeLines (colnames(input$dataCV) == colnames(new.data), con = writeOutputTarget )

  tmp.diff <- TRUE
  for( i in 1:NCOL(input$dataCV)){
    if( colnames(input$dataCV)[i] != colnames(new.data)[i] ){
      tmp.diff <- FALSE
    }
  }
  if( !tmp.diff && !is.null(writeOutputTarget) ){
    writeLines("WARNING, the columns of your input table have to be reordered to fit the given regression formula", con = writeOutputTarget )
    writeLines("Input of data:", con = writeOutputTarget )
    write.table(input$dataCV, file = writeOutputTarget, sep="\t", row.names = FALSE )
    writeLines( paste("New Sort of columns according to formula:",deparse(input$regressionFormula)), con = writeOutputTarget )
    write.table(new.data, file = writeOutputTarget, sep="\t", row.names = FALSE )
    writeLines("", con = writeOutputTarget )
  }
  return( as.data.frame(new.data) )
}

