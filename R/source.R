## Author: Ingo Feinerer
## Sources

getSources <- function()
   c("DataframeSource", "DirSource", "GmaneSource", "ReutersSource", "URISource", "VectorSource")

## Class definitions

setClass("Source",
         representation(DefaultReader = "function",
                        Encoding = "character",
                        Length = "numeric",
                        LoDSupport = "logical",
                        Position = "numeric",
                        Vectorized = "logical",
                        "VIRTUAL"),
         validity = function(object) {
             if (object@Vectorized && (object@Length <= 0))
                 return("Vectorized sources must have positive length")
             TRUE
         })

# A data frame where each row is interpreted as document
setClass("DataframeSource",
         representation(Content = "data.frame"),
         contains = c("Source"))

# A directory with files
setClass("DirSource",
         representation(FileList = "character"),
         contains = c("Source"))

# A single document identified by a Uniform Resource Identifier
setClass("URISource",
         representation(URI = "call"),
         contains = c("Source"))

# A vector where each component is interpreted as document
setClass("VectorSource",
         representation(Content = "vector"),
         contains = c("Source"))

# XML
setClass("XMLSource",
         representation(URI = "call",
                        Content = "list"),
         contains = c("Source"))

## Methods

setGeneric("VectorSource", function(object, encoding = "UTF-8") standardGeneric("VectorSource"))
setMethod("VectorSource",
          signature(object = "vector"),
          function(object, encoding = "UTF-8") {
              new("VectorSource", LoDSupport = FALSE, Content = object, Position = 0,
                  DefaultReader = readPlain, Encoding = encoding, Length = length(object),
                  Vectorized = TRUE)
          })

CSVSource <- function(object, encoding = "UTF-8")
    .Defunct("DataframeSource", package = "tm",
             msg = "'CSVSource' is defunct.\nUse 'DataframeSource(read.csv(..., stringsAsFactors = FALSE))' instead.\nSee help(\"Defunct\")")

DataframeSource <- function(object, encoding = "UTF-8")
    new("DataframeSource", LoDSupport = FALSE, Content = object, Position = 0,
        DefaultReader = readPlain, Encoding = encoding, Length = nrow(object),
        Vectorized = TRUE)

setGeneric("DirSource", function(directory, encoding = "UTF-8", pattern = NULL, recursive = FALSE, ignore.case = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, encoding = "UTF-8", pattern = NULL, recursive = FALSE, ignore.case = FALSE) {
              d <- dir(directory, full.names = TRUE, pattern = pattern, recursive = recursive, ignore.case = ignore.case)
              isdir <- sapply(d, file.info)["isdir",]
              files <- d[isdir == FALSE]
              new("DirSource", LoDSupport = TRUE, FileList = files, Position = 0,
                  DefaultReader = readPlain, Encoding = encoding, Length = length(files),
                  Vectorized = TRUE)
          })

setGeneric("URISource", function(object, encoding = "UTF-8") standardGeneric("URISource"))
setMethod("URISource", signature(object = "character"),
          function(object, encoding = "UTF-8")
              new("URISource", LoDSupport = TRUE, URI = substitute(file(object, encoding = encoding)),
                  Position = 0, DefaultReader = readPlain, Encoding = encoding, Length = 1, Vectorized = FALSE))
setMethod("URISource", signature(object = "ANY"),
          function(object, encoding = "UTF-8")
              new("URISource", LoDSupport = TRUE, URI = match.call()$object,
                  Position = 0, DefaultReader = readPlain, Encoding = encoding, Length = 1, Vectorized = FALSE))

GmaneSource <- function(object, encoding = "UTF-8")
    XMLSource(object,
              function(tree) {
                  root <- XML::xmlRoot(tree)
                  root$children[names(root$children) == "item"]
              },
              readGmane, encoding)

ReutersSource <- function(object, encoding = "UTF-8")
    XMLSource(object, function(tree) XML::xmlRoot(tree)$children, readReut21578XML, encoding)

XMLSource <- function(object, parser, reader, encoding = "UTF-8") {
    require("XML")

    corpus <- readLines(object, encoding = encoding)
    tree <- XML::xmlTreeParse(corpus, asText = TRUE)
    content <- parser(tree)
    uri <- if (is.character(object)) substitute(file(object, encoding = encoding)) else NULL

    new("XMLSource", LoDSupport = FALSE, URI = uri,
        Content = content, Position = 0, DefaultReader = reader,
        Encoding = encoding, Length = length(content), Vectorized = FALSE)
}

setGeneric("stepNext", function(object) standardGeneric("stepNext"))
setMethod("stepNext", signature(object = "Source"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })

setGeneric("getElem", function(object) standardGeneric("getElem"))
setMethod("getElem", signature(object = "VectorSource"),
          function(object) list(content = object@Content[object@Position], uri = NULL))
setMethod("getElem", signature(object = "DataframeSource"),
          function(object) list(content = object@Content[object@Position, ], uri = NULL))
setMethod("getElem",
          signature(object = "DirSource"),
          function(object) {
              filename <- object@FileList[object@Position]
              encoding <- object@Encoding
              list(content = readLines(filename, encoding = encoding),
                   uri = substitute(file(filename, encoding = encoding)))
          })
setMethod("getElem", signature(object = "URISource"),
          function(object) list(content = readLines(eval(object@URI)), uri = object@URI))
setMethod("getElem",
          signature(object = "XMLSource"),
          function(object) {
              require("XML")

              # Construct a character representation from the XMLNode
              virtual.file <- character(0)
              con <- textConnection("virtual.file", "w", local = TRUE)
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })

setGeneric("pGetElem", function(object) standardGeneric("pGetElem"))
setMethod("pGetElem", signature(object = "DataframeSource"),
          function(object) lapply(seq_len(object@Length),
                                  function(x) list(content = object@Content[x,], uri = NULL)))
setMethod("pGetElem", signature(object = "DirSource"),
          function(object) {
              lapply(object@FileList,
                     function(x) {
                         filename <- x
                         encoding <- object@Encoding
                         list(content = readLines(filename, encoding = encoding),
                              uri = substitute(file(filename, encoding = encoding)))
                     })
          })
setMethod("pGetElem", signature(object = "VectorSource"),
          function(object) lapply(object@Content, function(x) list(content = x, uri = NULL)))

setGeneric("eoi", function(object) standardGeneric("eoi"))
setMethod("eoi", signature(object = "VectorSource"),
          function(object) return(length(object@Content) <= object@Position))
setMethod("eoi", signature(object = "DataframeSource"),
          function(object) return(nrow(object@Content) <= object@Position))
setMethod("eoi", signature(object = "DirSource"),
          function(object) return(length(object@FileList) <= object@Position))
setMethod("eoi", signature(object = "URISource"),
          function(object) return(1 <= object@Position))
setMethod("eoi", signature(object = "XMLSource"),
          function(object) return(length(object@Content) <= object@Position))
