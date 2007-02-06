# Author: Ingo Feinerer
# Filters

getFilters <- function() {
   c("sFilter", "searchFullText", "tmIntersect")
}

setGeneric("searchFullText", function(object, pattern, ...) standardGeneric("searchFullText"))
setMethod("searchFullText",
          signature(object = "PlainTextDocument", pattern = "character"),
          function(object, pattern, ...) {
              any(grep(pattern, Content(object)))
          })

setGeneric("tmIntersect", function(object, words, ...) standardGeneric("tmIntersect"))
setMethod("tmIntersect",
          signature(object = "PlainTextDocument", words = "character"),
          function(object, words, ...) {
              length(intersect(names(termFreq(object)), words)) > 0
          })
