func.createSample <-
function( input ){
  n <- nrow(input$dataCV)
  #DEFAULT: Leave-One-Out cross validation, number of groups is equal to the number of elements 
  if( input$nGroup == n )
    return( 1:n )
  else
    return( sample(1:n) )
}

