func.output.PerformanceValues <-
function(result, output.round, writeOutputTarget){
  if( is.null(writeOutputTarget) )
    return()

  writeLines("\n---- RESULTS ----", con = writeOutputTarget)
  writeLines("", con = writeOutputTarget)

  writeLines( paste("-- FULL REGRESSION"), con = writeOutputTarget )
  writeLines( paste("#Elements: \t\t",result$r2$elements[1],sep=""), con = writeOutputTarget )
  writeLines("", con = writeOutputTarget)

  writeLines( paste("rms: \t\t\t", round(result$r2$rms[1], output.round),sep=""), con = writeOutputTarget )
  writeLines( paste("r^2 (use Y_mean): \t", round(result$r2$value[1], output.round),sep=""), con = writeOutputTarget )
  writeLines( "", con = writeOutputTarget )

  writeLines( paste("-- CROSS VALIDATION",sep=""), con = writeOutputTarget )
  #number of different test sets is missing
  writeLines( paste("Split Size Data Set: \t\t",result$cv$split_size_data_set[1],sep=""), con = writeOutputTarget )
  writeLines( paste("#Splitted Sets: \t\t",result$cv$splitted_sets_count[1],sep=""), con = writeOutputTarget )
  writeLines("", con = writeOutputTarget)
  
  writeLines( paste("#Elements Training Set: \t",result$cv$elements_training_set[1],sep=""), con = writeOutputTarget )
  writeLines( paste("#Elements Test Set: \t\t",result$cv$elements_test_set[1],sep=""), con = writeOutputTarget )
  rep.run <- result$cv$repeated_runs[1]
  if( rep.run > 0)
    writeLines( paste("different runs: \t\t\t", rep.run, sep="" ), con = writeOutputTarget )
  writeLines("", con = writeOutputTarget)

  writeLines( paste("rms: \t\t\t\t", round(result$cv$q2$rms[1], output.round),sep=""), con = writeOutputTarget )
#  writeLines( paste("q^2 (use Y_mean): \t\t\t\t\t", round(result$q2$value[1], output.round),sep=""), con = writeOutputTarget )
  writeLines( paste("q^2_cv (use Y_mean^training): \t", round(result$cv$q2$value[1], output.round),sep=""), con = writeOutputTarget )
  writeLines("\n---- End RESULTS ----", con = writeOutputTarget)
}

