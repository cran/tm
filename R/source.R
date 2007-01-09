# Author: Ingo Feinerer

# Source objects

setClass("Source",
         representation(LoDSupport = "logical",
                        Position = "numeric",
                        "VIRTUAL"))

# A directory with files
setClass("DirSource",
         representation(FileList = "character"),
         contains = c("Source"))

# A single CSV file where each line is interpreted as document
setClass("CSVSource",
         representation(URI = "ANY",
                        Content = "character"),
         contains = c("Source"))

# A single XML file consisting of several Reuters documents
# Works both for Reuters21578XML and RCV1 XML files
setClass("ReutersSource",
         representation(URI = "ANY",
                        Content = "list"),
         contains = c("Source"))

# A single XML (RDF) file containing Gmane mailing list archive feeds
setClass("GmaneSource",
         representation(URI = "ANY",
                        Content = "list"),
         contains = c("Source"))


# Methods for Source objects

setGeneric("DirSource", function(directory, recursive = FALSE) standardGeneric("DirSource"))
setMethod("DirSource",
          signature(directory = "character"),
          function(directory, recursive = FALSE) {
              d <- dir(directory, full.names = TRUE, recursive = recursive)
              isdir <- sapply(d, file.info)["isdir",]
              files <- d[isdir == FALSE]
              new("DirSource", LoDSupport = TRUE, FileList = files,
                  Position = 0)
          })

setGeneric("CSVSource", function(object) standardGeneric("CSVSource"))
setMethod("CSVSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("CSVSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              content <- scan(con, what = "character")
              close(con)
              new("CSVSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("ReutersSource", function(object) standardGeneric("ReutersSource"))
setMethod("ReutersSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("ReutersSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children

              new("ReutersSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("GmaneSource", function(object) standardGeneric("GmaneSource"))
setMethod("GmaneSource",
          signature(object = "character"),
          function(object) {
              object <- substitute(file(object))
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })
setMethod("GmaneSource",
          signature(object = "ANY"),
          function(object) {
              object <- substitute(object)
              con <- eval(object)
              corpus <- paste(readLines(con), "\n", collapse = "")
              close(con)
              tree <- xmlTreeParse(corpus, asText = TRUE)
              content <- xmlRoot(tree)$children
              content <- content[names(content) == "item"]

              new("GmaneSource", LoDSupport = FALSE, URI = object,
                  Content = content, Position = 0)
          })

setGeneric("stepNext", function(object) standardGeneric("stepNext"))
setMethod("stepNext",
          signature(object = "DirSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("stepNext",
          signature(object = "CSVSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("stepNext",
          signature(object = "ReutersSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })
setMethod("stepNext",
          signature(object = "GmaneSource"),
          function(object) {
              object@Position <- object@Position + 1
              object
          })

setGeneric("getElem", function(object) standardGeneric("getElem"))
setMethod("getElem",
          signature(object = "DirSource"),
          function(object) {
              filename <- object@FileList[object@Position]
              list(content = readLines(filename),
                   uri = substitute(file(filename)))
          })
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
              con <- textConnection("virtual.file", "w")
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })
setMethod("getElem",
          signature(object = "GmaneSource"),
          function(object) {
              # Construct a character representation from the XMLNode
              con <- textConnection("virtual.file", "w")
              saveXML(object@Content[[object@Position]], con)
              close(con)

              list(content = virtual.file, uri = object@URI)
          })

setGeneric("eoi", function(object) standardGeneric("eoi"))
setMethod("eoi",
          signature(object = "DirSource"),
          function(object) {
              if (length(object@FileList) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "CSVSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "ReutersSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
setMethod("eoi",
          signature(object = "GmaneSource"),
          function(object) {
              if (length(object@Content) <= object@Position)
                  return(TRUE)
              else
                  return(FALSE)
          })
