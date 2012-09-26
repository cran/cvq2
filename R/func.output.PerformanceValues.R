func.output.PerformanceValues <-
function(rmsFR, rmsFRElements, rSqFR, cvElements, cvRep, qSqCV, qSqCVSub, 
  rmsCV, rmsCVElements, output.round, writeOutputTarget
){
  writeLines("\n---- RESULTS ----", con = writeOutputTarget)
  writeLines("", con = writeOutputTarget)

  writeLines( paste("-- FULL REGRESSION"), con = writeOutputTarget )
  writeLines( paste("rms (all ",rmsFRElements," elements): \t", round(rmsFR, output.round),sep=""), con = writeOutputTarget )
  writeLines( paste("r^2: \t\t\t", round(rSqFR, output.round),sep=""), con = writeOutputTarget )
  writeLines( "", con = writeOutputTarget )
  
  writeLines( paste("-- CROSS VALIDATION",sep=""), con = writeOutputTarget )
  writeLines( paste("# leave out elements: \t\t\t",cvElements,sep=""), con = writeOutputTarget )
  if(cvRep > 0)
    writeLines( paste("different runs: \t\t\t",round(cvRep, output.round),sep=""), con = writeOutputTarget )

  writeLines( paste("rms (",rmsCVElements," elements): \t\t", round(rmsCV, output.round),sep=""), con = writeOutputTarget )
#  writeLines( paste("q^2 (mean(Y) same as for r^2): \t\t\t\t\t", round(qSqCV, output.round),sep=""), con = writeOutputTarget )
  writeLines( paste("q^2 (mean(Y) individual for each subset - excluding the leave one out row): \t", round(qSqCVSub, output.round),sep=""), con = writeOutputTarget )
  writeLines("\n---- End RESULTS ----", con = writeOutputTarget)
}

