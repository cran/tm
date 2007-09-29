# Author: Ingo Feinerer

# Function generator
setClass("FunctionGenerator",
         contains = "function")

# Constructor
setGeneric("FunctionGenerator", function(object) standardGeneric("FunctionGenerator"))
setMethod("FunctionGenerator",
          signature(object = "function"),
          function(object) {
              new("FunctionGenerator", .Data = object)
          })
