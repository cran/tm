.TextDocument <-
    function(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
{
    doc <- x
    attr(doc, "Author") <- as.character(author)
    attr(doc, "DateTimeStamp") <- as.POSIXlt(datetimestamp, tz = "GMT")
    attr(doc, "Description") <- as.character(description)
    attr(doc, "Heading") <- as.character(heading)
    attr(doc, "ID") <- as.character(id)
    attr(doc, "Language") <- as.character(language)
    attr(doc, "LocalMetaData") <- as.list(localmetadata)
    attr(doc, "Origin") <- as.character(origin)
    doc
}
Content <- function(x) UseMethod("Content", x)
`Content<-` <- function(x, value) UseMethod("Content<-", x)
`Content<-.default` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    mostattributes(x) <- attrs
    x
}
`Content<-.XMLDocument` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    mostattributes(x) <- attrs
    attr(x, "names") <- attr(value, "names")
    x
}
Author <- function(x) attr(x, "Author")
DateTimeStamp <- function(x) attr(x, "DateTimeStamp")
Description <- function(x) attr(x, "Description")
Heading <- function(x) attr(x, "Heading")
ID <- function(x) attr(x, "ID")
Language <- function(x) attr(x, "Language")
LocalMetaData <- function(x) attr(x, "LocalMetaData")
Origin <- function(x) attr(x, "Origin")

PlainTextDocument <-
    function(x = character(0), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(as.character(x), author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("PlainTextDocument", "TextDocument", "character")
    doc
}
print.PlainTextDocument <- function(x, ...) {
    cat(noquote(as.character(x)), sep = "\n")
    invisible(x)
}
Content.PlainTextDocument <- function(x) as.character(x)
`Content<-.PlainTextDocument` <- function(x, value) {
    attrs <- attributes(x)
    x <- as.character(value)
    attributes(x) <- attrs
    x
}

RCV1Document <-
    function(x = list(), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("RCV1Document", "TextDocument", "XMLDocument", "XMLAbstractDocument", "oldClass")
    doc
}

Reuters21578Document <-
    function(x = list(), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), heading = character(0), id = character(0), origin = character(0),
             language = character(0), localmetadata = list())
{
    doc <- .TextDocument(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
    class(doc) <- c("Reuters21578Document", "TextDocument", "XMLDocument", "XMLAbstractDocument", "oldClass")
    doc
}
