# Author: Ingo Feinerer

getSources <- function()
   c("CSVSource", "DataframeSource", "DirSource", "GmaneSource", "ReutersSource", "URISource", "VectorSource")

# Source objects

setClass("Source",
         representation(LoDSupport = "logical",
                        Position = "numeric",
                        DefaultReader = "function",
                        Encoding = "character",
                        Length = "numeric",
                        "VIRTUAL"))

# A vector where each component is interpreted as document
setClass("VectorSource",
         representation(Content = "vector"),
         contains = c("Source"))

# A data frame where each row is interpreted as document
setClass("DataframeSource",
         representation(Content = "data.frame"),
         contains = c("Source"))

# A single document identified by a Uniform Resource Identifier
setClass("URISource",
         representation(URI = "call"),
         contains = c("Source"))

# A directory with files
setClass("DirSource",
         representation(FileList = "character"),
         contains = c("Source"))

# A single CSV file where each line is interpreted as document
setClass("CSVSource",
         representation(URI = "call",
                        Content = "character"),
         contains = c("Source"))

# A single XML file consisting of several Reuters documents
# Works both for Reuters21578XML and RCV1 XML files
setClass("ReutersSource",
         representation(URI = "call",
                        Content = "list"),
         contains = c("Source"))

# A single XML (RDF) file containing Gmane mailing list archive feeds
setClass("GmaneSource",
         representation(URI = "call",
                        Content = "list"),
         contains = c("Source"))


# Methods for Source objects

setGeneric("VectorSource", function(object, encoding = "UTF-8") standardGeneric("VectorSource"))
setMethod("VectorSource",
          signature(object = "vector"),
          function(object, encoding = "UTF-8") {
              new("VectorSource", LoDSupport = FALSE, Content = object, Position = 0,
                  DefaultReader = readPlain, Encoding = encoding, Length = length(object))
          })

DataframeSource<- function(object, encoding = "UTF-8")
    new("DataframeSource", LoDSupport = FALSE, Content = object, Position = 0,
        DefaultReader = readPlain, Encoding = encoding, Length = nrow(object))

setGeneric("DirSource", function(directory, encoding = "UTF-8", recursive = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, encoding = "UTF-8", recursive = FALSE) {
              d <- dir(directory, full.names = TRUE, recursive = recursive)
              isdir <- sapply(d, file.info)["isdir",]
              files <- d[isdir == FALSE]
              new("DirSource", LoDSupport = TRUE, FileList = files,
                  Position = 0, DefaultReader = readPlain, Encoding = encoding, Length = length(files))
          })

setGeneric("URISource", function(object, encoding = "UTF-8") standardGeneric("URISource"))
setMethod("URISource", signature(object = "character"),
          function(object, encoding = "UTF-8")
              new("URISource", LoDSupport = TRUE, URI = substitute(file(object, encoding = encoding)),
                  Position = 0, DefaultReader = readPlain, Encoding = encoding, Length = 1))
setMethod("URISource", signature(object = "connection"),
          function(object, encoding = "UTF-8")
              new("URISource", LoDSupport = TRUE, URI = match.call()$object,
                  Position = 0, DefaultReader = readPlain, Encoding = encoding, Length = 1))

setGeneric("CSVSource", function(object, encoding = "UTF-8") standardGeneric("CSVSource"))
setMethod("CSVSource",
          signature(object = "character"),
          function(object, encoding = "UTF-8") {
              content <- apply(read.csv(object, encoding = encoding), 1, paste, collapse = " ")
              new("CSVSource", LoDSupport = FALSE, URI = substitute(file(object, encoding = encoding)),
                  Content = content, Position = 0, DefaultReader = readPlain,
                  Encoding = encoding, Length = length(content))
          })
setMethod("CSVSource",
          signature(object = "connection"),
          function(object, encoding = "UTF-8") {
              content <- apply(read.csv(object), 1, paste, collapse = " ")
              new("CSVSource", LoDSupport = FALSE, URI = match.call()$object,
                  Content = content, Position = 0, DefaultReader = readPlain,
                  Encoding = encoding, Length = length(content))
          })

setGeneric("ReutersSource", function(object, encoding = "UTF-8") standardGeneric("ReutersSource"))
setMethod("ReutersSource",
          signature(object = "character"),
          function(object, encoding = "UTF-8") {
              corpus <- paste(readLines(object, encoding = encoding), "\n", collapse = "")
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = substitute(file(object, encoding = encoding)),
                  Content = content, Position = 0, DefaultReader = readReut21578XML,
                  Encoding = encoding, Length = length(content))
          })
setMethod("ReutersSource",
          signature(object = "connection"),
          function(object, encoding = "UTF-8") {
              corpus <- paste(readLines(object), "\n", collapse = "")
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = match.call()$object,
                  Content = content, Position = 0, DefaultReader = readReut21578XML,
                  Encoding = encoding, Length = length(content))
          })

setGeneric("GmaneSource", function(object, encoding = "UTF-8") standardGeneric("GmaneSource"))
setMethod("GmaneSource",
          signature(object = "character"),
          function(object, encoding = "UTF-8") {
              corpus <- paste(readLines(object, encoding = encoding), "\n", collapse = "")
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneSource", LoDSupport = FALSE, URI = substitute(file(object, encoding = encoding)),
                  Content = content, Position = 0, DefaultReader = readGmane,
                  Encoding = encoding, Length = length(content))
          })
setMethod("GmaneSource",
          signature(object = "connection"),
          function(object, encoding = "UTF-8") {
              corpus <- paste(readLines(object), "\n", collapse = "")
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneSource", LoDSupport = FALSE, URI = match.call()$object,
                  Content = content, Position = 0, DefaultReader = readGmane,
                  Encoding = encoding, Length = length(content))
          })

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
          signature(object = "CSVSource"),
          function(object) {
              list(content = object@Content[object@Position],
                   uri = object@URI)
          })
setMethod("getElem",
          signature(object = "ReutersSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              virtual.file <- character(0)
              con <- textConnection("virtual.file", "w", local = TRUE)
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })
setMethod("getElem",
          signature(object = "GmaneSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              virtual.file <- character(0)
              con <- textConnection("virtual.file", "w", local = TRUE)
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })

setGeneric("eoi", function(object) standardGeneric("eoi"))
setMethod("eoi", signature(object = "VectorSource"),
          function(object) return(length(object@Content) <= object@Position))
setMethod("eoi", signature(object = "DataframeSource"),
          function(object) return(nrow(object@Content) <= object@Position))
setMethod("eoi", signature(object = "DirSource"),
          function(object) return(length(object@FileList) <= object@Position))
setMethod("eoi", signature(object = "URISource"),
          function(object) return(1 <= object@Position))
setMethod("eoi", signature(object = "CSVSource"),
          function(object) return(length(object@Content) <= object@Position))
setMethod("eoi", signature(object = "ReutersSource"),
          function(object) return(length(object@Content) <= object@Position))
setMethod("eoi", signature(object = "GmaneSource"),
          function(object) return(length(object@Content) <= object@Position))
