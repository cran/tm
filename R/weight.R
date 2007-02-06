# Author: Ingo Feinerer

# Weight Function Class
setClass("WeightFunction",
         representation(Name = "character"),
         contains = "function")

# Constructor
setGeneric("WeightFunction", function(object, name) standardGeneric("WeightFunction"))
setMethod("WeightFunction",
          signature(object = "function", name = "character"),
          function(object, name) {
              new("WeightFunction", .Data = object, Name = name)
          })

# Actual TermDocMatrix weighting functions
weightTf <- WeightFunction(function(m) m, "term frequency")
weightTfIdf <- WeightFunction(function(m) t(t(m) * log2(nrow(m) / colSums(as(m > 0, "dgCMatrix")))), "term frequency - inverse document frequency")
weightBin <- WeightFunction(function(m) as(m > 0, "dgCMatrix"), "binary")
weightLogical <- WeightFunction(function(m) m > 0, "logical")
