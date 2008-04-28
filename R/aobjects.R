# Author: Ingo Feinerer
# S4 class and accessor definitions

# Text document
setClass("TextDocument",
         representation(Author = "character",
                        DateTimeStamp = "POSIXt",
                        Description = "character",
                        ID = "character",
                        Origin = "character",
                        Heading = "character",
                        Language = "character",
                        LocalMetaData = "list",
                        "VIRTUAL"))

setGeneric("Author", function(object) standardGeneric("Author"))
setMethod("Author", "TextDocument", function(object) object@Author)
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))
setReplaceMethod("Author", "TextDocument", function(x, value) {
  x@Author <- value
  x
})

setGeneric("DateTimeStamp", function(object) standardGeneric("DateTimeStamp"))
setMethod("DateTimeStamp", "TextDocument", function(object) object@DateTimeStamp)
setGeneric("DateTimeStamp<-", function(x, value) standardGeneric("DateTimeStamp<-"))
setReplaceMethod("DateTimeStamp", "TextDocument", function(x, value) {
  x@DateTimeStamp <- value
  x
})

setGeneric("Description", function(object) standardGeneric("Description"))
setMethod("Description", "TextDocument", function(object) object@Description)
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))
setReplaceMethod("Description", "TextDocument", function(x, value) {
  x@Description <- value
  x
})

setGeneric("ID", function(object) standardGeneric("ID"))
setMethod("ID", "TextDocument", function(object) object@ID)
setGeneric("ID<-", function(x, value) standardGeneric("ID<-"))
setReplaceMethod("ID", "TextDocument", function(x, value) {
  x@ID <- value
  x
})

setGeneric("Origin", function(object) standardGeneric("Origin"))
setMethod("Origin", "TextDocument", function(object) object@Origin)
setGeneric("Origin<-", function(x, value) standardGeneric("Origin<-"))
setReplaceMethod("Origin", "TextDocument", function(x, value) {
  x@Origin <- value
  x
})

setGeneric("Heading", function(object) standardGeneric("Heading"))
setMethod("Heading", "TextDocument", function(object) object@Heading)
setGeneric("Heading<-", function(x, value) standardGeneric("Heading<-"))
setReplaceMethod("Heading", "TextDocument", function(x, value) {
  x@Heading <- value
  x
})

setGeneric("Language", function(object) standardGeneric("Language"))
setMethod("Language", "TextDocument", function(object) object@Language)
setGeneric("Language<-", function(x, value) standardGeneric("Language<-"))
setReplaceMethod("Language", "TextDocument", function(x, value) {
  x@Language <- value
  x
})

setGeneric("LocalMetaData", function(object) standardGeneric("LocalMetaData"))
setMethod("LocalMetaData", "TextDocument", function(object) object@LocalMetaData)

# Derived text documents

# Define class CallOrNULL as union of call and NULL
setClassUnion("callOrNULL", c("call", "NULL"))

# Plain text documents
setClass("PlainTextDocument",
         representation(URI = c("callOrNULL"), Cached = "logical"),
         contains = c("character", "TextDocument"))

setGeneric("Content", function(object) standardGeneric("Content"))
setMethod("Content", "PlainTextDocument", function(object) object@.Data)
setGeneric("Content<-", function(x, value) standardGeneric("Content<-"))
setReplaceMethod("Content", "PlainTextDocument", function(x, value) {
  x@.Data <- value
  x
})

setGeneric("URI", function(object) standardGeneric("URI"))
setMethod("URI", "PlainTextDocument", function(object) object@URI)

setGeneric("Cached", function(object) standardGeneric("Cached"))
setMethod("Cached", "PlainTextDocument", function(object) object@Cached)
setGeneric("Cached<-", function(x, value) standardGeneric("Cached<-"))
setReplaceMethod("Cached", "PlainTextDocument", function(x, value) {
  x@Cached <- value
  x
})

# XML text document
# If XMLDocument would be a S4 class, we could directly inherit from it
# Instead we have to do a work-around with a list
setClass("XMLTextDocument",
         representation(URI = "callOrNULL", Cached = "logical"),
         contains = c("list", "TextDocument"))

setMethod("Content", "XMLTextDocument", function(object) object@.Data)
setReplaceMethod("Content", "XMLTextDocument", function(x, value) {
    x@.Data <- value
    x
})
setMethod("URI", "XMLTextDocument", function(object) object@URI)
setMethod("Cached", "XMLTextDocument", function(object) object@Cached)
setReplaceMethod("Cached", "XMLTextDocument", function(x, value) {
    x@Cached <- value
    x
})

# Reuters21578 XML document
setClass("Reuters21578Document",
         contains = "XMLTextDocument")

# Reuters RCV1 document
setClass("RCV1Document",
         contains = "XMLTextDocument")

# Newsgroup document as found in the Newsgroup dataset of the UCI KDD archive
setClass("NewsgroupDocument",
         representation(Newsgroup = "character", URI = "callOrNULL", Cached = "logical"),
         contains = c("character", "TextDocument"))

setMethod("Content", "NewsgroupDocument", function(object) object@.Data)
setReplaceMethod("Content", "NewsgroupDocument", function(x, value) {
    x@.Data <- value
    x
})
setMethod("URI", "NewsgroupDocument", function(object) object@URI)
setMethod("Cached", "NewsgroupDocument", function(object) object@Cached)
setReplaceMethod("Cached", "NewsgroupDocument", function(x, value) {
  x@Cached <- value
  x
})

# Structured text document for sectioned or structured text corpora
setClass("StructuredTextDocument",
         representation(URI = "callOrNULL", Cached = "logical"),
         contains = c("list", "TextDocument"))

setMethod("Content", "StructuredTextDocument", function(object) object@.Data)
setReplaceMethod("Content", "StructuredTextDocument", function(x, value) {
    x@.Data <- value
    x
})
setMethod("URI", "StructuredTextDocument", function(object) object@URI)
setMethod("Cached", "StructuredTextDocument", function(object) object@Cached)
setReplaceMethod("Cached", "StructuredTextDocument", function(x, value) {
    x@Cached <- value
    x
})

# A node in the metadata tree of a corpus
setClass("MetaDataNode",
         representation(NodeID = "numeric",
                        MetaData = "list",
                        children = "list"))

# Corpus (= text document collection)
setClass("Corpus",
         representation(DMetaData = "data.frame", CMetaData = "MetaDataNode", DBControl = "list"),
         contains = c("list"))

# DMetaData = *MetaData* available for all *D*ocuments
setGeneric("DMetaData", function(object) standardGeneric("DMetaData"))
setMethod("DMetaData", "Corpus",
          function(object) {
              if (DBControl(object)[["useDb"]] && require("filehash")) {
                  db <- dbInit(DBControl(object)[["dbName"]], DBControl(object)[["dbType"]])
                  result <- dbFetch(db, "DMetaData")
                  index <- object@DMetaData[[1, "subset"]]
                  if (!any(is.na(index)))
                      result <- result[index, , drop = FALSE]
                  return(result)
              }
              else
                  return(object@DMetaData)
          })
setGeneric("DMetaData<-", function(x, value) standardGeneric("DMetaData<-"))
setReplaceMethod("DMetaData", "Corpus",
                 function(x, value) {
                     if (DBControl(x)[["useDb"]] && require("filehash")) {
                         db <- dbInit(DBControl(x)[["dbName"]], DBControl(x)[["dbType"]])
                         db[["DMetaData"]] <- value
                         x@DMetaData[[1, "subset"]] <- NA
                         return(x)
                     }
                     else {
                         x@DMetaData <- value
                         return(x)
                     }
                 })

# CMetaData = *MetaData* describing only the Document *C*ollection itself
setGeneric("CMetaData", function(object) standardGeneric("CMetaData"))
setMethod("CMetaData", "Corpus", function(object) object@CMetaData)

setGeneric("DBControl", function(object) standardGeneric("DBControl"))
setMethod("DBControl", "Corpus", function(object) object@DBControl)

# Repository for corpora
setClass("TextRepository",
         representation(RepoMetaData = "list"),
         contains = c("list"))

setGeneric("RepoMetaData", function(object) standardGeneric("RepoMetaData"))
setMethod("RepoMetaData", "TextRepository", function(object) object@RepoMetaData)
