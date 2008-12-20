# Author: Ingo Feinerer
# S4 class and accessor definitions
# Assignment and accessor functions are implemented as described in "S4 Classes in 15 pages, more or less"

# Text document
setClass("TextDocument",
         representation(Author = "character",
                        DateTimeStamp = "POSIXct",
                        Description = "character",
                        ID = "character",
                        Origin = "character",
                        Heading = "character",
                        Language = "character",
                        LocalMetaData = "list",
                        "VIRTUAL"))

if (!isGeneric("Author")) {
    if (is.function("Author"))
        fun <- Author
    else
        fun <- function(object) standardGeneric("Author")
    setGeneric("Author", fun)
}
setMethod("Author", "TextDocument", function(object) object@Author)
setGeneric("Author<-", function(x, value) standardGeneric("Author<-"))
setReplaceMethod("Author", "TextDocument", function(x, value) {
  x@Author <- value
  x
})

if (!isGeneric("DateTimeStamp")) {
    if (is.function("DateTimeStamp"))
        fun <- DateTimeStamp
    else
        fun <- function(object) standardGeneric("DateTimeStamp")
    setGeneric("DateTimeStamp", fun)
}
setMethod("DateTimeStamp", "TextDocument", function(object) object@DateTimeStamp)
setGeneric("DateTimeStamp<-", function(x, value) standardGeneric("DateTimeStamp<-"))
setReplaceMethod("DateTimeStamp", "TextDocument", function(x, value) {
  x@DateTimeStamp <- value
  x
})

if (!isGeneric("Description")) {
    if (is.function("Description"))
        fun <- Description
    else fun <- function(object) standardGeneric("Description")
    setGeneric("Description", fun)
}
setMethod("Description", "TextDocument", function(object) object@Description)
setGeneric("Description<-", function(x, value) standardGeneric("Description<-"))
setReplaceMethod("Description", "TextDocument", function(x, value) {
  x@Description <- value
  x
})

if (!isGeneric("ID")) {
    if (is.function("ID"))
        fun <- ID
    else fun <- function(object) standardGeneric("ID")
    setGeneric("ID", fun)
}
setMethod("ID", "TextDocument", function(object) object@ID)
setGeneric("ID<-", function(x, value) standardGeneric("ID<-"))
setReplaceMethod("ID", "TextDocument", function(x, value) {
  x@ID <- value
  x
})

if (!isGeneric("Origin")) {
    if (is.function("Origin"))
        fun <- Origin
    else fun <- function(object) standardGeneric("Origin")
    setGeneric("Origin", fun)
}
setMethod("Origin", "TextDocument", function(object) object@Origin)
setGeneric("Origin<-", function(x, value) standardGeneric("Origin<-"))
setReplaceMethod("Origin", "TextDocument", function(x, value) {
  x@Origin <- value
  x
})

if (!isGeneric("Heading")) {
    if (is.function("Heading"))
        fun <- Heading
    else fun <- function(object) standardGeneric("Heading")
    setGeneric("Heading", fun)
}
setMethod("Heading", "TextDocument", function(object) object@Heading)
setGeneric("Heading<-", function(x, value) standardGeneric("Heading<-"))
setReplaceMethod("Heading", "TextDocument", function(x, value) {
  x@Heading <- value
  x
})

if (!isGeneric("Language")) {
    if (is.function("Language"))
        fun <- Language
    else fun <- function(object) standardGeneric("Language")
    setGeneric("Language", fun)
}
setMethod("Language", "TextDocument", function(object) object@Language)
setGeneric("Language<-", function(x, value) standardGeneric("Language<-"))
setReplaceMethod("Language", "TextDocument", function(x, value) {
  x@Language <- value
  x
})

if (!isGeneric("LocalMetaData")) {
    if (is.function("LocalMetaData"))
        fun <- LocalMetaData
    else fun <- function(object) standardGeneric("LocalMetaData")
    setGeneric("LocalMetaData", fun)
}
setMethod("LocalMetaData", "TextDocument", function(object) object@LocalMetaData)

# Inherited text documents

# Define class CallOrNULL as union of call and NULL
setClassUnion("callOrNULL", c("call", "NULL"))

# Plain text documents
setClass("PlainTextDocument",
         representation(URI = c("callOrNULL"), Cached = "logical"),
         contains = c("character", "TextDocument"))

if (!isGeneric("Content")) {
    if (is.function("Content"))
        fun <- Content
    else
        fun <- function(object) standardGeneric("Content")
    setGeneric("Content", fun)
}
setMethod("Content", "PlainTextDocument", function(object) object@.Data)
setGeneric("Content<-", function(x, value) standardGeneric("Content<-"))
setReplaceMethod("Content", "PlainTextDocument", function(x, value) {
  x@.Data <- value
  x
})

if (!isGeneric("URI")) {
    if (is.function("URI"))
        fun <- URI
    else fun <- function(object) standardGeneric("URI")
    setGeneric("URI", fun)
}
setMethod("URI", "PlainTextDocument", function(object) object@URI)

if (!isGeneric("Cached")) {
    if (is.function("Cached"))
        fun <- Cached
    else fun <- function(object) standardGeneric("Cached")
    setGeneric("Cached", fun)
}
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

# A node in the metadata tree of a text document collection
setClass("MetaDataNode",
         representation(NodeID = "numeric",
                        MetaData = "list",
                        children = "list"))

# Text document collection
setClass("Corpus",
         representation(DMetaData = "data.frame", CMetaData = "MetaDataNode", DBControl = "list"),
         contains = c("list"))

# DMetaData = *MetaData* available for all *D*ocuments
if (!isGeneric("DMetaData")) {
    if (is.function("DMetaData"))
        fun <- DMetaData
    else fun <- function(object) standardGeneric("DMetaData")
    setGeneric("DMetaData", fun)
}
setMethod("DMetaData", "Corpus",
          function(object) {
              if (DBControl(object)[["useDb"]]) {
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
                     if (DBControl(x)[["useDb"]]) {
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
if (!isGeneric("CMetaData")) {
    if (is.function("CMetaData"))
        fun <- CMetaData
    else fun <- function(object) standardGeneric("CMetaData")
    setGeneric("CMetaData", fun)
}
setMethod("CMetaData", "Corpus", function(object) object@CMetaData)

if (!isGeneric("DBControl")) {
    if (is.function("DBControl"))
        fun <- DBControl
    else fun <- function(object) standardGeneric("DBControl")
    setGeneric("DBControl", fun)
}
setMethod("DBControl", "Corpus", function(object) object@DBControl)

# Repository for text document collections
setClass("TextRepository",
         representation(RepoMetaData = "list"),
         contains = c("list"))

if (!isGeneric("RepoMetaData")) {
    if (is.function("RepoMetaData"))
        fun <- RepoMetaData
    else fun <- function(object) standardGeneric("RepoMetaData")
    setGeneric("RepoMetaData", fun)
}
setMethod("RepoMetaData", "TextRepository", function(object) object@RepoMetaData)

# Term-document matrix
setClass("TermDocMatrix",
         representation(Data = "Matrix", Weighting = "character"))

if (!isGeneric("Data")) {
    if (is.function("Data"))
        fun <- Data
    else
        fun <- function(object) standardGeneric("Data")
    setGeneric("Data", fun)
}
setMethod("Data", "TermDocMatrix", function(object) object@Data)
setGeneric("Data<-", function(x, value) standardGeneric("Data<-"))
setReplaceMethod("Data", "TermDocMatrix", function(x, value) {
  x@Data <- value
  x
})

if (!isGeneric("Weighting")) {
    if (is.function("Weighting"))
        fun <- Weighting
    else
        fun <- function(object) standardGeneric("Weighting")
    setGeneric("Weighting", fun)
}
setMethod("Weighting", "TermDocMatrix", function(object) object@Weighting)
setGeneric("Weighting<-", function(x, value) standardGeneric("Weighting<-"))
setReplaceMethod("Weighting", "TermDocMatrix", function(x, value) {
  x@Weighting <- value
  x
})
