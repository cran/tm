## Author: Ingo Feinerer
## Sources

getSources <- function()
   c("DataframeSource", "DirSource", "GmaneSource", "ReutersSource", "URISource", "VectorSource")

.Source <- function(defaultreader, encoding, length, lodsupport, position, vectorized) {
    if (vectorized && (length <= 0))
        stop("Vectorized sources must have positive length")

    structure(list(DefaultReader = defaultreader, Encoding = encoding, Length = length,
                   LoDSupport = lodsupport, Position = position, Vectorized = vectorized),
              class = "Source")
}

# A vector where each component is interpreted as document
VectorSource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, length(x), FALSE, 0, TRUE)
    s$Content <- x
    class(s) = c("VectorSource", "Source")
    s
}

CSVSource <- function(x, encoding = "UTF-8")
    .Defunct("DataframeSource", package = "tm",
             msg = "'CSVSource' is defunct.\nUse 'DataframeSource(read.csv(..., stringsAsFactors = FALSE))' instead.\nSee help(\"Defunct\")")

# A data frame where each row is interpreted as document
DataframeSource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, nrow(x), FALSE, 0, TRUE)
    s$Content <- x
    class(s) = c("DataframeSource", "Source")
    s
}

# A directory with files
DirSource <- function(directory, encoding = "UTF-8", pattern = NULL, recursive = FALSE, ignore.case = FALSE) {
    d <- dir(directory, full.names = TRUE, pattern = pattern, recursive = recursive, ignore.case = ignore.case)
    isdir <- sapply(d, file.info)["isdir",]
    files <- d[isdir == FALSE]

    s <- .Source(readPlain, encoding, length(files), TRUE, 0, TRUE)
    s$FileList <- files
    class(s) = c("DirSource", "Source")
    s
}

# A single document identified by a Uniform Resource Identifier
URISource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, 1, TRUE, 0, FALSE)
    s$URI <- match.call()$x
    class(s) = c("URISource", "Source")
    s
}

GmaneSource <- function(x, encoding = "UTF-8")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree))[names(XML::xmlChildren(XML::xmlRoot(tree))) == "item"], readGmane, encoding)

ReutersSource <- function(x, encoding = "UTF-8")
    XMLSource(x, function(tree) XML::xmlChildren(XML::xmlRoot(tree)), readReut21578XML, encoding)

# XML
XMLSource <- function(x, parser, reader, encoding = "UTF-8") {
    corpus <- readLines(x, encoding = encoding)
    tree <- XML::xmlTreeParse(corpus, asText = TRUE)
    content <- parser(tree)

    s <- .Source(reader, encoding, length(content), FALSE, 0, FALSE)
    s$Content <- content
    s$URI <- match.call()$x
    class(s) = c("XMLSource", "Source")
    s
}

stepNext <- function(x) UseMethod("stepNext", x)
stepNext.Source <- function(x) {
    x$Position <- x$Position + 1
    x
}

getElem <- function(x) UseMethod("getElem", x)
getElem.DataframeSource <- function(x) list(content = x$Content[x$Position, ], uri = match.call()$x)
getElem.DirSource <- function(x) {
    filename <- x$FileList[x$Position]
    encoding <- x$Encoding
    list(content = readLines(filename, encoding = encoding), uri = filename)
}
getElem.URISource <- function(x) list(content = readLines(eval(x$URI)), uri = x$URI)
getElem.VectorSource <- function(x) list(content = x$Content[x$Position], uri = match.call()$x)
getElem.XMLSource <- function(x) {
    # Construct a character representation from the XMLNode
    virtual.file <- character(0)
    con <- textConnection("virtual.file", "w", local = TRUE)
    XML::saveXML(x$Content[[x$Position]], con)
    close(con)

    list(content = virtual.file, uri = x$URI)
}

pGetElem <- function(x) UseMethod("pGetElem", x)
pGetElem.DataframeSource <- function(x)
    lapply(seq_len(x$Length), function(y) list(content = x$Content[y,], uri = match.call()$x))
pGetElem.DirSource <- function(x)
    lapply(x$FileList, function(y) list(content = readLines(y, encoding = x$Encoding), uri = y))
pGetElem.VectorSource <- function(x)
    lapply(x$Content, function(y) list(content = y, uri = match.call()$x))

eoi <- function(x) UseMethod("eoi", x)
eoi.DataframeSource <- function(x) nrow(x$Content) <= x$Position
eoi.DirSource <- function(x) length(x$FileList) <= x$Position
eoi.URISource <- function(x) 1 <= x$Position
eoi.VectorSource <- eoi.XMLSource <- function(x) length(x$Content) <= x$Position
