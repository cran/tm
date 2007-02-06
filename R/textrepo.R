# Author: Ingo Feinerer

setGeneric("TextRepository", function(object, meta = list(created = Sys.time())) standardGeneric("TextRepository"))
setMethod("TextRepository",
          signature(object = "Corpus"),
          function(object, meta) {
              return(new("TextRepository", .Data = list(object), RepoMetaData = meta))
          })

setMethod("appendElem",
          signature(object = "TextRepository", data = "Corpus"),
          function(object, data, meta = NULL) {
              object[[length(object)+1]] <- data
              object@RepoMetaData <- c(object@RepoMetaData, meta)
              return(object)
          })

setMethod("appendMeta",
          signature(object = "TextRepository"),
          function(object, cmeta = NULL, dmeta = NULL) {
              object@RepoMetaData <- c(object@RepoMetaData, cmeta)
              return(object)
          })

setMethod("removeMeta",
          signature(object = "TextRepository"),
          function(object, cname = NULL, dname = NULL) {
              if (!is.null(cname))
                  object@RepoMetaData <- RepoMetaData(object)[names(RepoMetaData(object)) != cname]
              return(object)
          })

setMethod("length",
          signature(x = "TextRepository"),
          function(x){
              return(length(as(x, "list")))
    })

setMethod("show",
          signature(object = "TextRepository"),
          function(object){
               cat(sprintf(ngettext(length(object),
                                    "A text repository with %d text document collection\n",
                                    "A text repository with %d text document collections\n"),
                           length(object)))
    })

setMethod("summary",
          signature(object = "TextRepository"),
          function(object){
              show(object)
              if (length(RepoMetaData(object)) > 0) {
                  cat(sprintf(ngettext(length(RepoMetaData(object)),
                                              "\nThe repository metadata consists of %d tag-value pair\n",
                                              "\nThe repository metadata consists of %d tag-value pairs\n"),
                                       length(RepoMetaData(object))))
                  cat("Available tags are:\n")
                  cat(names(RepoMetaData(object)), "\n")
              }
    })
