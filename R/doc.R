# Register S3 document classes to be recognized by S4 methods. This is
# mainly a fix to be compatible with packages which were originally
# developed to cooperate with corresponding S4 tm classes. Necessary
# since tm's class architecture was changed to S3 since tm version 0.5.
setOldClass(c("PlainTextDocument", "TextDocument", "character"))

.TextDocument <-
    function(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
{
    doc <- x
    attr(doc, "Author") <- author
    attr(doc, "DateTimeStamp") <- datetimestamp
    attr(doc, "Description") <- description
    attr(doc, "Heading") <- heading
    attr(doc, "ID") <- id
    attr(doc, "Language") <- language
    attr(doc, "LocalMetaData") <- localmetadata
    attr(doc, "Origin") <- origin
    doc
}
Content <- function(x) UseMethod("Content", x)
`Content<-` <- function(x, value) UseMethod("Content<-", x)
`Content<-.default` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
    x
}
`Content<-.XMLDocument` <- function(x, value) {
    attrs <- attributes(x)
    x <- value
    attributes(x) <- attrs
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
