## Author: Ingo Feinerer
## Sources

getSources <- function()
   c("DataframeSource", "DirSource", "GmaneSource", "ReutersSource", "URISource", "VectorSource")

.Source <- function(defaultreader, encoding, length, lodsupport, names, position, vectorized, class = NULL) {
    if (vectorized && (length <= 0))
        stop("vectorized sources must have positive length")

    if (!is.null(names) && (length != length(names)))
        stop("incorrect number of element names")

    structure(list(DefaultReader = defaultreader, Encoding = encoding, Length = length,
                   LoDSupport = lodsupport, Names = names, Position = position, Vectorized = vectorized),
              class = unique(c(class, "Source")))
}

# A vector where each component is interpreted as document
VectorSource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, length(x), FALSE, names(x), 0, TRUE, class = "VectorSource")
    s$Content <- x
    s
}

# A data frame where each row is interpreted as document
DataframeSource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, nrow(x), FALSE, row.names(x), 0, TRUE, class = "DataframeSource")
    s$Content <- x
    s
}

# A directory with files
DirSource <- function(directory = ".", encoding = "UTF-8", pattern = NULL, recursive = FALSE, ignore.case = FALSE) {
    d <- dir(directory, full.names = TRUE, pattern = pattern, recursive = recursive, ignore.case = ignore.case)

    if (length(d) == 0)
        stop("empty directory")

    isfile <- logical(length(d))
    for (i in seq_along(d))
      isfile[i] <- !file.info(d[i])["isdir"]

    s <- .Source(readPlain, encoding, sum(isfile), TRUE, basename(d[isfile]), 0, TRUE, class = "DirSource")
    s$FileList <- d[isfile]
    s
}

# A single document identified by a Uniform Resource Identifier
URISource <- function(x, encoding = "UTF-8") {
    s <- .Source(readPlain, encoding, 1, TRUE, NULL, 0, FALSE, class = "URISource")
    s$URI <- x
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

    s <- .Source(reader, encoding, length(content), FALSE, NULL, 0, FALSE, class = "XMLSource")
    s$Content <- content
    s$URI <- x
    s
}

stepNext <- function(x) UseMethod("stepNext", x)
stepNext.Source <- function(x) {
    x$Position <- x$Position + 1
    x
}

getElem <- function(x) UseMethod("getElem", x)
getElem.DataframeSource <- function(x) list(content = x$Content[x$Position, ])
getElem.DirSource <- function(x) {
    filename <- x$FileList[x$Position]
    encoding <- x$Encoding
    list(content = readLines(filename, encoding = encoding), uri = filename)
}
getElem.URISource <- function(x) list(content = readLines(x$URI, encoding = x$Encoding), uri = x$URI)
getElem.VectorSource <- function(x) list(content = x$Content[x$Position])
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
    lapply(seq_len(x$Length), function(y) list(content = x$Content[y,]))
pGetElem.DirSource <- function(x)
    lapply(x$FileList, function(y) list(content = readLines(y, encoding = x$Encoding), uri = y))
pGetElem.VectorSource <- function(x)
    lapply(x$Content, function(y) list(content = y))

eoi <- function(x) UseMethod("eoi", x)
eoi.DataframeSource <- function(x) nrow(x$Content) <= x$Position
eoi.DirSource <- function(x) length(x$FileList) <= x$Position
eoi.URISource <- function(x) 1 <= x$Position
eoi.VectorSource <- eoi.XMLSource <- function(x) length(x$Content) <= x$Position
