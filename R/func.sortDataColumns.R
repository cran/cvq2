func.sortDataColumns <-
function(tmp.data, tmp.formula, writeOutputTarget){
  new.data <- cbind(tmp.data[,all.vars(tmp.formula)[2]])
  colnames(new.data) <- all.vars(tmp.formula)[2]

  if( NCOL(tmp.data) > 2 ){
    for( i in 3:NCOL(tmp.data)){
      new.data <- cbind(new.data, tmp.data[,all.vars(tmp.formula)[i]] )
      colnames(new.data)[i-1] <- all.vars(tmp.formula)[i]
    }
  }
#  colnames(new.data) <- all.vars(tmp.formula)

  # y as last column
  new.data <- cbind(new.data, tmp.data[,all.vars(tmp.formula)[1]])
  colnames(new.data)[NCOL(tmp.data)] <- all.vars(tmp.formula)[1]

  rownames(new.data) <- 1:NROW(new.data)
#  writeLines (colnames(tmp.data) == colnames(new.data), con = writeOutputTarget )

  tmp.diff <- TRUE
  for( i in 1:NCOL(tmp.data)){
    if( colnames(tmp.data)[i] != colnames(new.data)[i] ){
      tmp.diff <- FALSE
    }
  }
  if( !tmp.diff && !is.null(writeOutputTarget) ){
    writeLines("WARNING, the columns of your input table have to be reordered to fit the given regression formula", con = writeOutputTarget )
    writeLines("Input of data:", con = writeOutputTarget )
    write.table(tmp.data, file = writeOutputTarget, sep="\t", row.names = FALSE )
    writeLines( paste("New Sort of columns according to formula:",deparse(tmp.formula)), con = writeOutputTarget )
    write.table(new.data, file = writeOutputTarget, sep="\t", row.names = FALSE )
    writeLines("", con = writeOutputTarget )
  }
  as.data.frame(new.data)
}

