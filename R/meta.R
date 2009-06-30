# Author: Ingo Feinerer

setGeneric("meta", function(object, tag, type = NULL) standardGeneric("meta"))
setMethod("meta",
          signature(object = "Corpus"),
          function(object, tag, type = c("indexed", "corpus", "local")) {
              type <- match.arg(type)
              if (identical(type, "indexed"))
                  return(DMetaData(object)[tag])
              if (missing(tag) && identical(type, "corpus"))
                  return(CMetaData(object))
              if (identical(type, "corpus"))
                  return(CMetaData(object)@MetaData[[tag]])
              if (missing(tag) && identical(type, "local"))
                  return(invisible(lapply(object, meta)))
              if (identical(type, "local"))
                  return(lapply(object, meta, tag))
          })
setMethod("meta",
          signature(object = "TextRepository"),
          function(object, tag, type = NULL) {
              if (missing(tag))
                  RepoMetaData(object)
              else
                  RepoMetaData(object)[[tag]]
          })
setMethod("meta",
          signature(object = "TextDocument"),
          function(object, tag, type = NULL) {
              if (missing(tag)) {
                  slots <- sort(setdiff(slotNames(object), c(".Data", "LocalMetaData")))
                  cat("Available meta data pairs are:\n")
                  for (s in slots)
                      cat(sprintf("  %-13s: %s\n", s, paste(as(slot(object, s), "character"), collapse = " ")))
                  if (length(LocalMetaData(object)) > 0) {
                      cat("User-defined local meta data pairs are:\n")
                      show(LocalMetaData(object))
                  }
              }
              else {
                  if (tag %in% slotNames(object)) slot(object, tag)
                  else LocalMetaData(object)[[tag]]
              }
          })
#setMethod("meta",
#          signature(object = "MinimalDocument"),
#          function(object, tag, type = NULL) {
#              if (missing(tag)) {
#                  cat("Available meta data pairs are:\n")
#                  for (s in c("ID", "Language"))
#                      cat(sprintf("  %-8s: %s\n", s, paste(as(attr(object, s), "character"), collapse = " ")))
#              }
#              else
#                  attr(object, tag)
#          })

setGeneric("meta<-", function(object, tag, type = NULL, value) standardGeneric("meta<-"))
setReplaceMethod("meta",
                 signature(object = "Corpus"),
                 function(object, tag, type = "indexed", value) {
                     if ((type != "indexed") && (type != "corpus") && (type != "local"))
                         stop("invalid type")
                     if (type == "indexed")
                         DMetaData(object)[, tag] <- value
                     else if (type == "local")
                         for (i in seq_along(object))
                             meta(object[[i]], tag) <- value[[i]]
                     else # (type == "corpus")
                         object@CMetaData@MetaData[[tag]] <- value
                     object
                 })
setReplaceMethod("meta",
                 signature(object = "TextRepository"),
                 function(object, tag, type = NULL, value) {
                     object@RepoMetaData[[tag]] <- value
                     object
})
setReplaceMethod("meta",
                 signature(object = "TextDocument"),
                 function(object, tag, type = NULL, value) {
                     if (tag %in% setdiff(slotNames(object), ".Data"))
                         slot(object, tag) <- value
                     else
                         object@LocalMetaData[[tag]] <- value
                     object
})

# Simple Dublin Core to tm meta data mappings
# http://en.wikipedia.org/wiki/Dublin_core#Simple_Dublin_Core
DublinCoretm <- function(DCElem) {
    if (DCElem == "Title") return(list(tag = "Heading", type = "local"))
    if (DCElem == "Creator") return(list(tag = "Author", type = "local"))
    if (DCElem == "Description") return(list(tag = "Description", type = "local"))
    if (DCElem == "Date") return(list(tag = "DateTimeStamp", type = "local"))
    if (DCElem == "Identifier") return(list(tag = "ID", type = "local"))
    if (DCElem == "Language") return(list(tag = "Language", type = "local"))
    # Source -> Origin ?

    if (DCElem == "Subject" || DCElem == "Publisher" || DCElem == "Contributor" ||
        DCElem == "Type" || DCElem == "Format" || DCElem == "Source" ||
        DCElem == "Relation" || DCElem == "Coverage" || DCElem == "Rights")
        return(list(tag = DCElem, type = "extended"))

    stop("invalid simple Dublin Core meta data element")
}

setGeneric("DublinCore", function(object, tag = NULL) standardGeneric("DublinCore"))
setMethod("DublinCore",
          signature(object = "TextDocument"),
          function(object, tag = NULL) {
              if (is.null(tag)) {
                  elements <- c("Title", "Creator", "Subject", "Description", "Publisher",
                                "Contributor", "Date", "Type", "Format", "Identifier",
                                "Source", "Language", "Relation", "Coverage", "Rights")
                  cat("Simple Dublin Core meta data pairs are:\n")
                  for (e in elements) {
                      DCtm <- DublinCoretm(e)
                      DCvalue <- if (DCtm$type == "local") slot(object, DCtm$tag)
                      else LocalMetaData(object)[[DCtm$tag]]
                      cat(sprintf("  %-11s: %s\n", e, paste(as(DCvalue, "character"), collapse = " ")))
                  }
              }
              else {
                  DCtm <- DublinCoretm(tag)
                  if (DCtm$type == "local") show(slot(object, DCtm$tag))
                  else show(LocalMetaData(object)[[DCtm$tag]])
              }
          })

setGeneric("DublinCore<-", function(object, tag, value) standardGeneric("DublinCore<-"))
setReplaceMethod("DublinCore",
                 signature(object = "TextDocument"),
                 function(object, tag, value) {
                     DCtm <- DublinCoretm(tag)
                     if (DCtm$type == "local") slot(object, DCtm$tag) <- value
                     else object@LocalMetaData[[DCtm$tag]] <- value
                     object
})
