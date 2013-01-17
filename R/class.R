#setGeneric("cvq2")

#class definition
setClass( "cvq2", representation( result="list", output="list" ) )
setMethod("show", "cvq2", function(object) func.output.performanceValues(object@result, object@output) )

#  setMethod("[", "cvq2", function(x, i, j, ..., drop) { x@result <- x@result[i]; x })
#  setMethod("@",...) ???
